% ============================================================================
% CONSTRAINT STORY: second_amendment_text__collective_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__collective_security_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_text__collective_security_reading
 *   human_readable: Second Amendment Text — Collective Security Reading
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   The Second Amendment text reads: 'A well regulated Militia, being
 *   necessary to the security of a free State, the right of the people to
 *   keep and bear Arms, shall not be infringed.' This story instantiates ONE
 *   READING of the contested kernel (the text itself). The
 *   collective-security reading interprets the militia clause as CONDITIONING
 *   the operative clause: the right to keep and bear arms is secured because
 *   a well-regulated militia is necessary to state security, and the state
 *   may therefore regulate arms (via licensing, background checks,
 *   permitting) to serve that collective security function. Under this
 *   reading, the state licensing authority is a structural beneficiary — it
 *   gains regulatory authority over a constitutionally protected domain.
 *   Individual gun owners become a constrained class subject to state
 *   permission. The constraint operates as TANGLED ROPE: genuine coordination
 *   function (state-citizen militia capacity, law enforcement efficiency) AND
 *   asymmetric extraction (licensing authority, permitting discretion,
 *   registration requirements that constrain individual autonomy). Active
 *   enforcement is required — unlicensed ownership is a crime; confiscation
 *   is the penalty. Sibling readings (individual_right and
 *   originalist_civic_virtue) interpret the same text differently and
 *   instantiate different constraints with different beneficiary/victim
 *   structures; they are NOT part of this story. The claim/metric gap is
 *   intentional: the reading is CLAIMED as tangled_rope (the state's own
 *   framing of coordination + licensing function), and the metrics describe
 *   substantially extractive, actively enforced operation with rising theater
 *   over time (more performative security justification, less actual
 *   enforcement gain). The engine measures that divergence; the committer
 *   frame preserves the reading distinction through omega variables.
 *
 * KEY AGENTS:
 *   - state_licensing_authority: institutional agenda-setter, sets permitting standards and enforcement, collects administrative power and fees
 *   - law_enforcement_agencies: institutional beneficiary, operates under collective-security framing that justifies registration and pre-screening
 *   - unlicensed_gun_owners: powerless, identity-locked payer, cannot acquire firearms without state permission
 *   - rural_subsistence_hunters: powerless, constrained payer, face licensing burden unrecognized by collective-security reading
 *   - marginalized_populations: powerless, trapped payer, encounter discriminatory permitting denial and criminal penalties for non-compliance
 *   - constitutional_originalists: excluded powerful seat, disputes reading on originalist grounds, influences through litigation
 *   - individual_right_advocates: excluded organized seat, disputes reading on operative-clause grounds, mounts resistance through courts and advocacy
 *   - legal_scholars_collective_security: analytical seat, grounds the reading in constitutional interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, 0.68).
domain_priors:suppression_score(second_amendment_text__collective_security_reading, 0.72).
domain_priors:theater_ratio(second_amendment_text__collective_security_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__collective_security_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__collective_security_reading, "Second Amendment Text — Collective Security Reading").
narrative_ontology:topic_domain(second_amendment_text__collective_security_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(second_amendment_text__collective_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__collective_security_reading, '79d10b81-bafe-4f90-860a-e244be53a8fd').
narrative_ontology:cs_kernel_codification('79d10b81-bafe-4f90-860a-e244be53a8fd', fixed_text).
narrative_ontology:cs_authority_grounding('79d10b81-bafe-4f90-860a-e244be53a8fd', extraction).
narrative_ontology:cs_interpretation_layer_present('79d10b81-bafe-4f90-860a-e244be53a8fd').
narrative_ontology:cs_reading_relation('79d10b81-bafe-4f90-860a-e244be53a8fd', second_amendment_text__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('79d10b81-bafe-4f90-860a-e244be53a8fd', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('79d10b81-bafe-4f90-860a-e244be53a8fd', foundational, militia_clause_conditions_operative_clause).
narrative_ontology:cs_axiom_status(militia_clause_conditions_operative_clause, holdable).
narrative_ontology:cs_axiom_grounding('79d10b81-bafe-4f90-860a-e244be53a8fd', militia_clause_conditions_operative_clause, empirically_contingent).
narrative_ontology:cs_axiom('79d10b81-bafe-4f90-860a-e244be53a8fd', secondary, state_licensing_serves_collective_security).
narrative_ontology:cs_axiom_status(state_licensing_serves_collective_security, holdable).
narrative_ontology:cs_axiom_grounding('79d10b81-bafe-4f90-860a-e244be53a8fd', state_licensing_serves_collective_security, instrumental).
narrative_ontology:cs_reference_frame('79d10b81-bafe-4f90-860a-e244be53a8fd', constitutional_state_regulatory_authority).
narrative_ontology:cs_drift_state('79d10b81-bafe-4f90-860a-e244be53a8fd', contemporary_divergence_from_founding, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('79d10b81-bafe-4f90-860a-e244be53a8fd', '').
narrative_ontology:cs_kernel_id(second_amendment_text__collective_security_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, state_licensing_authority).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, law_enforcement_agencies).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, unlicensed_gun_owners).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, rural_subsistence_hunters).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, marginalized_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers permitting, licensing, and regulatory enforcement for firearms under the collective-security reading. Claims authority to condition individual access on demonstration of responsible use and compatibility with state militia/law enforcement coordination. Sets licensing standards, conducts background checks, and can deny permits deemed inconsistent with public order. Collects administrative fees and exercises discretionary authority over market entry. The collective-security reading legitimates this role by connecting it to militia necessity and state security.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, state_licensing_authority, agenda_setter,
    institutional, generational, analytical, national).

% Operates under a constitutional reading that positions gun regulation as serving law enforcement capacity and public order. Background-check systems, felony bars, and registration operate in law enforcement's favor. The collective-security reading provides constitutional justification for these mechanisms. Receives data from licensing authority and gains pre-screening power unavailable under alternative readings.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, law_enforcement_agencies, beneficiary,
    institutional, generational, analytical, national).

% Cannot legally acquire, possess, or carry firearms without state permission under this reading. If they own firearms acquired before licensing regimes or in permissive prior jurisdictions, they must either comply with new licensing, surrender weapons, or operate illegally, carrying criminal penalties. Gun ownership may be identity-constitutive (hunter, rural defender, self-reliant individual); exit means abandoning that identity or moving jurisdictions. Identity-lock binds them to the constraint.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, unlicensed_gun_owners, payer,
    powerless, biographical, identity_locked, national).

% Face permitting, licensing, and compliance costs that are higher relative to income in low-population regions where licensing infrastructure is sparse or distant. Licensing delays can make seasonal hunting windows inaccessible. The subsistence function (food provision, predator control for livestock) is not recognized under the collective-security reading as a special case justifying exemption. They absorb regulatory costs without the power to negotiate terms.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, rural_subsistence_hunters, payer,
    powerless, biographical, constrained, regional).

% Encounter higher denial rates in permitting systems due to criminal-justice contact, residency instability, and documented disparate impacts in licensing administration. The collective-security reading's emphasis on background checks and 'fitness' determinations operates through licensing authorities with discretionary power; they cannot appeal denials effectively or understand permit requirements without legal support. Trapped because they cannot exit the requirement, cannot reliably access the permitting process, and non-compliance is penalized with felony criminal liability.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, marginalized_populations, payer,
    powerless, biographical, trapped, national).

% Advocate the originalist_civic_virtue_reading: the founding-era militia clause protected the right of citizens to maintain armed capacity independent of state licensing, understood as civic responsibility and universal militia participation. They are excluded from agenda-setting in licensing policy under the collective-security reading; their objections on originalist-historical grounds are not seated in regulatory authority design. They can influence jurisprudence through litigation and amicus testimony but cannot participate in or veto licensing policy design.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, constitutional_originalists, excluded,
    powerful, generational, mobile, national).

% Argue the individual_right_reading: the operative clause ('the right of the people to keep and bear Arms, shall not be infringed') protects personal self-defense and other lawful purposes independent of militia duty. The militia clause is merely prefatory, providing context but not conditioning. They are excluded from licensing policy design under the collective-security reading. They mount organized resistance through litigation (challenging licensing schemes), legislative advocacy, and public education but have no seat in regulatory design.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, individual_right_advocates, excluded,
    organized, generational, mobile, national).

% Produce constitutional interpretation grounding the collective-security reading in the text: the prefatory militia clause conditions the operative clause, and state regulatory authority is constitutionally justified. They generate supporting scholarship, provide expert testimony in litigation, and defend the reading against originalist and individual-right critiques. Analytical seat: they see the constraint from outside its enforcement but produce the interpretive scaffolding that legitimates it.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, legal_scholars_collective_security, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__collective_security_reading, state_licensing_authority).
narrative_ontology:fixing_cost_class(second_amendment_text__collective_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates citizen-state capacity for collective defense and law enforcement by creating information asymmetry that enables state vetting: the licensing system generates reliable knowledge of who holds firearms (for emergency mobilization, preventing criminal stockpiling, locating unregistered weapons). Positions individual gun owners as subject to state assessment of 'fitness' for militia participation. Distributes enforcement burden across citizens (they must comply with permitting) and state (licensing authority conducts background checks and maintains registries).
% TRANSFER_FUNCTION: Moves regulatory authority from individual discretion to state apparatus: the right to bear arms becomes conditional on state permission, which the state can grant, withhold, delay, or revoke based on licensing criteria. Transfers compliance burden and administrative cost to individual applicants (licensing fees, application delays, opportunity cost of permitting process). Transfers information (firearm registries, background-check data, ownership records) from private domain to law enforcement.
% ABSENT_VOICES: Constitutional originalists and individual-right advocates are excluded from licensing design and policy; subsistence hunters and rural communities without political leverage are not consulted on how licensing burden impacts their practices; marginalized communities with criminal-justice histories are not seated at authority tables. Their position: the collective-security reading misconstrues the militia clause as conditioning rather than contextualizing the operative clause, and licensing regimes are extractive mechanisms disguised as coordination.
% DISAPPEARANCE_RATIONALE: If the collective-security reading and its licensing apparatus vanished overnight, firearm access would shift to individual-market or state-monopoly terms depending on which reading prevails. Registration and background-check systems would collapse unless replaced by alternative mechanisms. State law-enforcement capacity to pre-screen gun ownership would be removed entirely. The political-constitutional landscape would realign rapidly to either the individual-right reading (minimal state regulation) or originalist reading (universal militia capacity without licensing). Constitutional jurisprudence would be forced to adjudicate which reading dominates.
% FOUNDING_PROBLEM: A well-regulated militia is necessary to the security of a free state; citizens armed for collective defense require coordination and fitness assessment to prevent infiltration by seditious or criminal elements and to ensure the militia functions as legitimate civic institution rather than armed mob or destabilizing force.
% FOUNDING_PROBLEM_CORROBORATION: State licensing authorities and law enforcement attest the founding problem is live and licensing regimes are the solution—background checks prevent gun crime, registration enables emergency mobilization, permitting screens out felons and the dangerously unfit. Constitutional originalists and individual-right advocates attest that the founding problem was about preventing federal MONOPOLY on armed force, not about authorizing state-licensing of individual ownership; they cite founding-era records showing militia was universal armed citizenry, not state-controlled apparatus. Contemporary criminologists dispute whether background-check systems meaningfully reduce gun crime relative to the licensing burden imposed. No consensus outside the state-benefiting parties (licensing authority, law enforcement) on whether the problem statement reflects the founding clause or is a modern reconstruction.
narrative_ontology:disappearance_verdict(second_amendment_text__collective_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__collective_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__collective_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_text__collective_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__collective_security_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__collective_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__collective_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68 (end-of-interval): the licensing regime conditions access to a constitutional right on state permission, permitting authority can deny based on criteria that may not track the stated coordination function, and compliance burden falls on those least able to bear it (marginalized, rural, subsistence populations). Suppression is higher (0.72): the constraint requires active enforcement — unlicensed ownership is criminalized, confiscation is the penalty, and state can monitor/confiscate in ways that have no analog in pure coordination. Theater is rising over the interval (0.22 to 0.41): early in the interval, licensing is presented as security screening with genuine coordination value; by the end, the same apparatus is maintained through rhetoric about public safety while research shows background checks are only partially effective and permitting denial is increasingly discretionary/political. The measurement series document extraction accumulation: the extractiveness increases from 0.48 to 0.68 over 35 years as licensing regimes expand and permitting standards tighten, while theater ratios rise (more performance, less function). Suppression plateaus at 0.72 — the enforcement capacity is mature and stable, not intensifying, but not declining either. This pattern is consistent with tangled rope that is slowly converting toward snare: the coordination function (militia, law enforcement efficiency) is real but the asymmetric extraction (licensing authority, state control) is expanding and the rationale is increasingly theatrical. One shared time grid across all three metrics ensures alignment.
 *
 * PERSPECTIVAL GAP:
 *   The state_licensing_authority and law_enforcement_agencies compute the constraint as genuine coordination with permissible extraction (their seat): the collective-security reading justifies licensing as necessary to state defense, background checks prevent crime, and regulation is the price of rights protection. The unlicensed_gun_owners and rural_subsistence_hunters compute it as pure extraction (their seat): the reading misconstrues the operative clause, licensing is a pretense for state monopoly power, and permitting denies them access to a constitutionally protected right. Originalists and individual_right_advocates would compute it as snare (their reading): the collective-security reading is a constitutional misinterpretation; it forecloses the operative clause's independent meaning and converts a guarantee into a state permission-system. The engine computes these divergences from structural data — different power atoms, different exit options (identity_locked vs. analytical), different roles (payer vs. beneficiary). The authored claim (tangled_rope) sits between pure coordination and pure extraction, which is why the metrics show both functions present but asymmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   State_licensing_authority: d near 0.0 (full beneficiary) — sets rules, collects authority and fees, faces no exit constraint. Law_enforcement_agencies: d near 0.1 (light beneficiary) — operate under the reading's justification, gain information and authority, but are not the primary extractors. Unlicensed_gun_owners and rural_subsistence_hunters: d near 0.85 (heavy targets) — denied access, must comply or face criminalization, cannot exit the requirement except by leaving jurisdiction (mobile exit only). Marginalized_populations: d near 0.95 (full target) — trapped by criminal-justice contact history, face discriminatory denial, cannot reliably access permitting, non-compliance is penalized severely. Individual_right_advocates and originalists: d near 0.5 (symmetric) — they benefit from any expansion of gun rights but are excluded from agenda-setting, so they bear the cost of operating within a reading they reject. Directionality feeds the engine's χ computation: targets with high d and constrained/trapped exit see high effective extraction; beneficiaries with low d and arbitrage/analytical exit see low or negative χ. The collective-security reading's beneficiaries are mostly institutional (low d) and organized (low d); its targets are mostly individual, powerless, and identity-locked (high d).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy: the original mandate (well-regulated militia as necessary to free-state security) is contested as the founding problem status shows. Law enforcement attests the mandate is live — they argue background checks prevent crime and permitting ensures reliable militia participation. But originalists and individual-right scholars dispute that the founding problem supports state licensing; they argue the founding problem was preventing federal monopoly, not enabling state licensing. The constraint would be mandatrophy if the mandate (collective security via state licensing) was built to solve a founding problem that no longer exists and has not been replaced by a new function. But licensing also serves law-enforcement crime prevention, which is a live function today (even if secondary). The theater_ratio rising suggests some performative maintenance: the security justification is being stretched beyond what background checks actually achieve. However, the constraint is not yet pure Piton because enforcement is still functional (suppression remains at 0.72, not declining) and beneficiaries still exist (state authority, law enforcement). The classification is TANGLED ROPE moving toward SNARE, not yet PITON. Mandatrophy would be declared only if the founding-militia problem became completely displaced by a security problem that did NOT require licensing, and the licensing apparatus persisted purely for state extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_vs_conditioning_clause,
    'Does the militia clause CONDITION the operative clause (as this reading holds) or merely provide CONTEXT for it?',
    'Historical linguistics and constitutional interpretation comparing founding-era usage of ''being necessary'' constructions; textual analysis of whether prefatory clauses in founding documents typically narrow or contextualize operative clauses; jurisprudential track record of how courts have treated similar militia-clause framings in state constitutions.',
    'If prefatory: the individual_right_reading is correct and the state cannot use the militia clause to justify licensing regimes that restrict individual access. If conditioning: this reading is correct and licensing is constitutionally permissible. The constraint would shift from tangled_rope (asymmetric extraction with coordination) to snare (pure extraction with coordination as cover) under individual_right dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prefatory_vs_conditioning_clause, empirical, 'Textual ambiguity: does militia clause condition operative clause or provide prefatory context?').

omega_variable(
    well_regulated_militia_referent,
    'What did ''a well regulated Militia'' refer to in founding-era context — state-controlled militia (National Guard analogue) or the universal armed citizenry?',
    'Historical record of founding-era militia organization and practice; Federalist Papers and constitutional-convention debates; state constitutions and militia laws of the period; scholarly consensus on militia structure before National Guard professionalization (1880s-1900s).',
    'If state-controlled militia: the collective_security reading is grounded in founding intent — the militia clause does justify state licensing. If universal armed citizenry: the originalist_civic_virtue_reading is correct and the state cannot use ''militia'' to justify licensing of individual ownership. The constraint would decompose into two readings with opposite extraction profiles.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(well_regulated_militia_referent, empirical, 'Historical referent ambiguity: did militia clause refer to state-controlled force or universal citizens?').

omega_variable(
    extraction_mechanism_identity_fusion,
    'To what extent is the measured extraction (d = 0.85–0.95 for gun owners) driven by structural barriers (licensing authority, permitting denial) versus internalized identity fusion (gun ownership as constitutive identity, exit meaning abandonment of self)?',
    'Post-licensing-removal trajectories: if gun owners in jurisdictions with minimal licensing still resist using firearms differently (e.g., decline to carry when legally free to do so), internalization is substantial; if they immediately change behavior, suppression is mostly structural.',
    'If internalized: the constraint''s effective suppression is higher than the authored 0.72 suggests — the target carries the suppression with them after barrier removal. Classification would be closer to snare (pure extraction with suppression running deep). If mostly structural: the constraint is genuinely tangled_rope — extraction persists through institutional design, not psychological capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_mechanism_identity_fusion, empirical, 'Suppression mechanism: structural barriers versus internalized identity-lock.').

omega_variable(
    reading_coexistence_foreclosure,
    'Can the collective_security_reading and the individual_right_reading coexist within a single constitutional framework, or does one logically foreclose the other?',
    'Jurisprudential analysis: do courts maintain doctrinal space for both readings (coexists_with) or does domination by one reading logically exclude the other (forecloses)? United States v. Miller, District of Columbia v. Heller, and New York State Rifle & Pistol Association v. Bruen jurisprudence provide signals on whether courts treat the readings as alternative legitimate interpretations or mutually exclusive constitutional claims.',
    'If foreclosure: the kernel is unstable and one reading will eventually prevail juridically; the constraint''s classification is contingent on which reading wins. If coexistence: both readings remain live and the constraint is genuinely contested; classification remains tangled_rope (both coordination and extraction functions are live because both readings can motivate policy). The certainty of the measurement changes based on foreclosure verdict.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_coexistence_foreclosure, conceptual, 'Logical relationship between readings: do they coexist or does one foreclose the other?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__collective_security_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_text__collective_security_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(seco_tr_t5, second_amendment_text__collective_security_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement(seco_tr_t10, second_amendment_text__collective_security_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(seco_tr_t15, second_amendment_text__collective_security_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(seco_tr_t20, second_amendment_text__collective_security_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(seco_tr_t25, second_amendment_text__collective_security_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(seco_tr_t30, second_amendment_text__collective_security_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(seco_tr_t35, second_amendment_text__collective_security_reading, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_text__collective_security_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(seco_be_t5, second_amendment_text__collective_security_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(seco_be_t10, second_amendment_text__collective_security_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(seco_be_t15, second_amendment_text__collective_security_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(seco_be_t20, second_amendment_text__collective_security_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(seco_be_t25, second_amendment_text__collective_security_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(seco_be_t30, second_amendment_text__collective_security_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(seco_be_t35, second_amendment_text__collective_security_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_text__collective_security_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(seco_su_t5, second_amendment_text__collective_security_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(seco_su_t10, second_amendment_text__collective_security_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(seco_su_t15, second_amendment_text__collective_security_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(seco_su_t20, second_amendment_text__collective_security_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(seco_su_t25, second_amendment_text__collective_security_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(seco_su_t30, second_amendment_text__collective_security_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(seco_su_t35, second_amendment_text__collective_security_reading, suppression_requirement, 35, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__collective_security_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_text__collective_security_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% The Second Amendment kernel decomposes into THREE constraint stories, one per reading. Each reading instantiates a different constraint with different ε values, beneficiary/victim structures, and typologies. The collective_security_reading (this story) positions state licensing authority as beneficiary and individual gun owners as payers. The individual_right_reading positions gun owners as beneficiaries and state-licensing as having no justified basis under the operative clause. The originalist_civic_virtue_reading positions armed citizens as beneficiaries and state-licensing-monopoly as an illegitimate narrowing of founding-era militia capacity. All three share the same kernel (the text) and the same interval but have incompatible beneficiary/victim orderings and ε values. The network edges above link this reading to its siblings; all three must be present in the corpus for the full kernel to be modeled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_text__collective_security_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
