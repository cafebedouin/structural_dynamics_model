% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__collective_right_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: second_amendment_arms_right__collective_right_reading
 *   human_readable: Second Amendment as State Militia Prerogative (Collective Right Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the collective-right reading of the Second
 *   Amendment kernel: the clause protects state authority to maintain
 *   organized militias against federal disarmament, and does not confer an
 *   independent individual right to keep and bear arms outside that militia
 *   context. Under this reading, state governments and organized militia
 *   structures (later institutionalized as the National Guard) are the
 *   rights-holders; individual firearms possession is subject to ordinary
 *   legislative regulation, constrained only by whatever police-power limits
 *   apply generally, not by a special constitutional shield. This reading
 *   dominated federal appellate jurisprudence (e.g., United States v.
 *   Miller's militia-nexus framing) for much of the twentieth century until
 *   it was displaced at the Supreme Court level by District of Columbia v.
 *   Heller (2008), which adopted the individual-right reading instead. This
 *   story models the collective-right reading on its own terms as a distinct
 *   constraint, not as a rejected alternative — its ε is low and stable
 *   because, within its own logic, prohibition and regulation of individual
 *   possession outside militia service is not extraction at all but ordinary
 *   exercise of retained state police power.
 *
 * KEY AGENTS:
 *   - state_governments: agenda_setter (institutional/analytical) — administer militia structures and regulatory authority
 *   - state_organized_militias: beneficiary (organized/constrained) — the entity whose armament the clause protects
 *   - individual_gun_owners_outside_militia_service: payer (moderate/trapped) — bear the regulatory consequence of losing constitutional cover
 *   - constitutional_historians: analytical observer — assess the drafting-era evidence without adjudicating the live contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__collective_right_reading, 0.18).
domain_priors:suppression_score(second_amendment_arms_right__collective_right_reading, 0.35).
domain_priors:theater_ratio(second_amendment_arms_right__collective_right_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__collective_right_reading, "Second Amendment as State Militia Prerogative (Collective Right Reading)").
narrative_ontology:topic_domain(second_amendment_arms_right__collective_right_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__collective_right_reading, 'fbf8d711-b15e-42f0-bea3-c7e3faaa16aa').
narrative_ontology:cs_kernel_codification('fbf8d711-b15e-42f0-bea3-c7e3faaa16aa', fixed_text).
narrative_ontology:cs_authority_grounding('fbf8d711-b15e-42f0-bea3-c7e3faaa16aa', lineage).
narrative_ontology:cs_interpretation_layer_present('fbf8d711-b15e-42f0-bea3-c7e3faaa16aa').
narrative_ontology:cs_reading_relation('fbf8d711-b15e-42f0-bea3-c7e3faaa16aa', second_amendment_arms_right__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('fbf8d711-b15e-42f0-bea3-c7e3faaa16aa', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('fbf8d711-b15e-42f0-bea3-c7e3faaa16aa', foundational, prefatory_clause_limits_operative_right).
narrative_ontology:cs_axiom_status(prefatory_clause_limits_operative_right, holdable).
narrative_ontology:cs_axiom_grounding('fbf8d711-b15e-42f0-bea3-c7e3faaa16aa', prefatory_clause_limits_operative_right, conventional).
narrative_ontology:cs_axiom('fbf8d711-b15e-42f0-bea3-c7e3faaa16aa', foundational, states_are_the_rights_holders).
narrative_ontology:cs_axiom_status(states_are_the_rights_holders, holdable).
narrative_ontology:cs_axiom_grounding('fbf8d711-b15e-42f0-bea3-c7e3faaa16aa', states_are_the_rights_holders, conventional).
narrative_ontology:cs_axiom('fbf8d711-b15e-42f0-bea3-c7e3faaa16aa', secondary, individual_possession_is_ordinary_police_power_subject).
narrative_ontology:cs_axiom_status(individual_possession_is_ordinary_police_power_subject, overridden).
narrative_ontology:cs_axiom_grounding('fbf8d711-b15e-42f0-bea3-c7e3faaa16aa', individual_possession_is_ordinary_police_power_subject, conventional).
narrative_ontology:cs_reference_frame('fbf8d711-b15e-42f0-bea3-c7e3faaa16aa', militia_dependent_right_framework).
narrative_ontology:cs_drift_state('fbf8d711-b15e-42f0-bea3-c7e3faaa16aa', post_heller_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('fbf8d711-b15e-42f0-bea3-c7e3faaa16aa', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_organized_militias).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, gun_regulation_proponents).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, individual_gun_owners_outside_militia_service).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, firearms_retailers).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__collective_right_reading, state_police_power_primacy).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__collective_right_reading, federalism_over_individual_arms_liberty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, states are the actual rights-holders: the amendment protects their sovereign authority to organize, arm, and control a militia against federal disarmament. States administer militia (now largely National Guard) structures and can regulate private firearms possession broadly outside that context, since the constitutional protection does not reach individuals acting independently of state military organization.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, state_governments, agenda_setter,
    institutional, generational, analytical, national).

% Historically the state militia (now institutionalized as the National Guard) is the entity whose armament and organizational autonomy the clause was written to secure against federal preemption. Members bear arms in service of a state-authorized structure, not as an independent personal liberty; their claim to protection runs through the state's authority, not their own standing.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, state_organized_militias, beneficiary,
    organized, generational, constrained, regional).

% Advocates for firearms regulation benefit from this reading because it removes constitutional cover for individual ownership claims, leaving broad legislative latitude to restrict, license, or prohibit private firearms possession without triggering an individual-rights constitutional challenge.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, gun_regulation_proponents, beneficiary,
    organized, biographical, mobile, national).

% Persons who own or wish to own firearms for self-defense, hunting, or other private purposes, with no organized militia affiliation, find that under this reading their possession claims carry no independent constitutional weight. They are subject to whatever regulatory regime state or federal legislatures enact; their only recourse is the ordinary political process, not judicial enforcement of a personal constitutional right. Exit from the constraint's reach is not possible short of relocating to a permissive jurisdiction or accepting the regulatory burden.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, individual_gun_owners_outside_militia_service, payer,
    moderate, biographical, trapped, national).

% Commercial sellers of firearms bear compliance costs and market contraction risk under regimes justified by this reading, since sales to non-militia individuals receive no constitutional insulation from restrictive licensing, taxation, or prohibition regimes.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, firearms_retailers, payer,
    moderate, biographical, constrained, national).

% The clause's original target was federal power to disarm state militias; the federal government today sits in an ambiguous position — neither the clear beneficiary nor bearer under this reading, but historically the entity the right was drafted to constrain. It is largely absent from the modern debate over the collective-right reading, which is fought mainly between state regulatory authority and individual claimants.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_government, excluded,
    institutional, generational, analytical, national).

% Study the drafting history, militia clauses in Article I, and ratification-era debates to assess whether the amendment's text and context support state-centered or individual-centered readings. Their scholarship is invoked by all sides but does not itself adjudicate the live doctrinal contest.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__collective_right_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__collective_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Secures state authority to organize and arm a militia independent of federal control, preventing the federal government from disarming state defense structures and thereby preserving a decentralized check on federal military monopoly.
% TRANSFER_FUNCTION: Moves the locus of constitutional protection from individual persons to state institutions: state governments and their organized militias retain a insulated sphere of authority, while individual claims to armed possession outside that structure receive no comparable constitutional insulation and are transferred into the ordinary space of legislative discretion.
% ABSENT_VOICES: Individual gun owners with no militia affiliation would object that this reading strips a personal liberty they believe the text protects independently; they are present in political and litigation contexts but structurally excluded from standing under this reading's own logic, since the right as construed here is not theirs to assert.
% DISAPPEARANCE_RATIONALE: If this reading of the clause vanished (i.e., were uniformly rejected by all courts and legislatures), state governments would lose one doctrinal basis for firearms regulation but would likely retain broad police-power authority to regulate arms through other constitutional doctrines; individual owners would gain a potential doctrinal foothold only if the individual-right reading correspondingly prevailed. Whether the world meaningfully rearranges depends on which sibling reading fills the vacuum, which is precisely the contested question the kernel poses.
% FOUNDING_PROBLEM: The founding-era problem was preventing a newly empowered federal government from disarming state militias and thereby neutralizing states' capacity for self-defense and resistance to federal overreach, in the aftermath of anti-Federalist fears about a standing federal army.
% FOUNDING_PROBLEM_CORROBORATION: Some constitutional historians and state governments attest the militia-protection problem remains structurally analogous today (federal-state balance of coercive force), corroborated by scholarship on the Militia Clauses in Article I, Section 8. Individual-rights scholars and firearms-rights organizations dispute this characterization entirely, arguing the historical record supports an individual liberty distinct from militia service; this dispute is the kernel contest itself, not resolved by either side's self-interested account.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__collective_right_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_arms_right__collective_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__collective_right_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_arms_right__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__collective_right_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__collective_right_reading_tests).
:- end_tests(second_amendment_arms_right__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) because, within this reading's own frame, restricting individual possession outside militia service is not extraction from a rights-holder — individuals were never the rights-holders under this account, so regulation of their conduct does not draw down a constitutional entitlement they hold. Suppression is moderate (0.35) reflecting the real coercive apparatus (licensing regimes, prohibitions, criminal enforcement) applied against individual possession where this reading has prevailed doctrinally, notably in the pre-Heller circuit courts. Theater ratio is modest (0.22): enforcement under this reading is substantially functional (actual restriction of civilian possession) rather than performative, though some jurisdictions layer symbolic gestures atop functional regulation. Resistance is high (0.72) because the reading has been aggressively contested by individual-rights advocates, gun-rights organizations, and ultimately displaced at the Supreme Court level — a reading facing this much sustained doctrinal challenge cannot be scored as meeting little resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the state-government seat, this reading looks like ordinary and legitimate exercise of retained police power over a domain (weapons regulation) the federal Constitution never removed from state competence. From the individual-owner seat, the identical doctrinal structure looks like the wholesale withdrawal of a liberty they believe the constitutional text independently protects. The engine's per-seat computation should reflect this asymmetry: the state seat likely computes near rope or scaffold (legitimate coordination, low extraction), while the individual-owner seat may compute nearer snare or tangled_rope territory depending on how heavily state police power is exercised in practice — the same structural facts, different seats, different classifications, and neither is wrong within its own frame.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and organized militias sit at the beneficiary end: the clause's protective function runs to them, and their exit options are effectively moot since they hold the authority the clause secures rather than being subject to it. Individual gun owners sit at the target end: under this reading, the constitutional shield simply does not reach them, so any regulatory burden falls with full force and no doctrinal recourse — hence trapped exit options and payer role. Firearms retailers are secondary payers, bearing compliance costs derivative of the individuals-are-unprotected structure. The federal government occupies an ambiguous seat: historically the entity constrained by the right, but not clearly a beneficiary or victim of the collective-right reading as applied today, since modern disputes are mostly state-versus-individual rather than state-versus-federal.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's founding problem — federal disarmament of state militias — is substantially dead as a live practical concern in the modern era: no serious contemporary proposal threatens to federally disarm state National Guard units, and the militia system itself has been thoroughly absorbed into the federal-state hybrid structure of Title 32 National Guard service. Yet the doctrinal reading persists as a live position in constitutional argument, invoked less to solve the original problem than to support contemporary firearms regulation goals. This is a candidate for founding_problem_status: contested rather than flatly dead, because some scholars argue the federal-state balance-of-force concern retains modern analytical value even if the specific 1791 fear (disarmed state militias facing a federal standing army) is no longer operative in its original form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_operative_clause_relationship,
    'Does the prefatory militia clause (''A well regulated Militia...'') limit the scope of the operative clause (''the right of the people...''), or does it merely state a purpose without narrowing the right''s holders?',
    'Resolution would require either a definitive historical-linguistic consensus on eighteenth-century legal drafting conventions for justificatory clauses, or a stable, non-overturned line of Supreme Court precedent settling the interpretive question. Neither currently exists — Heller (2008) rejected this reading at the federal constitutional level but did not resolve the underlying historical-linguistic dispute, which remains contested among originalist scholars themselves.',
    'If the prefatory clause is found to be genuinely limiting, this reading''s structural claim (state militia as rights-holder) gains strong textual support and the individual_right_reading''s foreclosure risk increases. If the prefatory clause is found to be non-limiting, this reading''s core premise collapses and the constraint dissolves into the individual_right_reading''s domain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prefatory_clause_operative_clause_relationship, conceptual, 'Whether the prefatory militia clause limits the operative right''s scope or merely announces purpose.').

omega_variable(
    militia_referent_modern_correspondence,
    'Does the modern National Guard structure genuinely correspond to the founding-era ''militia'' the clause protects, or has the concept been institutionally transformed beyond recognition (federalized, professionalized, no longer composed of the general armed citizenry)?',
    'Historical-institutional analysis comparing the unorganized/organized militia distinction under the Militia Acts of 1792 and 1903 against the current Title 10/Title 32 National Guard dual-status structure; corroboration from military historians outside constitutional law.',
    'If the modern militia bears little structural resemblance to the founding referent, the collective-right reading''s contemporary application becomes anachronistic, weakening its founding_problem_status claim to liveness and strengthening arguments that any doctrinal work it does today is a repurposed function rather than continuity of the original coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_referent_modern_correspondence, empirical, 'Whether the modern National Guard is a genuine continuation of the founding-era militia referent.').

omega_variable(
    state_police_power_vs_constructed_convenience,
    'Is the collective-right reading a genuine originalist recovery of the clause''s meaning, or a doctrinally convenient construction that emerged primarily because it authorized preferred firearms regulation outcomes?',
    'Comparative analysis of pre-twentieth-century judicial and scholarly commentary on the amendment (before organized regulatory interest in the reading existed) against post-1900s advocacy-driven scholarship, checking whether the collective-right interpretation predates the political motivation to adopt it.',
    'If the reading emerged primarily as advocacy-driven construction rather than pre-existing interpretive consensus, its claim to being the historically correct reading (as opposed to one convenient reading among several) weakens considerably, and its beneficiaries (state governments, regulation proponents) would look more like architects of a favorable framing than discoverers of an original meaning — this is the natural-law-vs-constructed ambiguity this Mountain-adjacent-but-not-Mountain reading must confront, since it declares beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_police_power_vs_constructed_convenience, conceptual, 'Whether the collective-right reading is genuine historical recovery or advocacy-driven doctrinal construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__collective_right_reading, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_arms_right__collective_right_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_arms_right__collective_right_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_arms_right__collective_right_reading, theater_ratio, 1939, 0.2).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_arms_right__collective_right_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_arms_right__collective_right_reading, theater_ratio, 2008, 0.3).
narrative_ontology:measurement(seco_tr_t2026, second_amendment_arms_right__collective_right_reading, theater_ratio, 2026, 0.22).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1791, 0.05).
narrative_ontology:measurement(seco_be_t1900, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1900, 0.08).
narrative_ontology:measurement(seco_be_t1939, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1939, 0.12).
narrative_ontology:measurement(seco_be_t1980, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(seco_be_t2008, second_amendment_arms_right__collective_right_reading, base_extractiveness, 2008, 0.2).
narrative_ontology:measurement(seco_be_t2026, second_amendment_arms_right__collective_right_reading, base_extractiveness, 2026, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1791, 0.15).
narrative_ontology:measurement(seco_su_t1900, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1900, 0.2).
narrative_ontology:measurement(seco_su_t1939, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1939, 0.3).
narrative_ontology:measurement(seco_su_t1980, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1980, 0.28).
narrative_ontology:measurement(seco_su_t2008, second_amendment_arms_right__collective_right_reading, suppression_requirement, 2008, 0.45).
narrative_ontology:measurement(seco_su_t2026, second_amendment_arms_right__collective_right_reading, suppression_requirement, 2026, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__collective_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, civic_republican_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the colloquial label 'the Second Amendment right' per the ε-invariance principle: collective_right_reading (this story, low ε, state-centered), individual_right_reading (a distinct constraint claiming higher ε if read as broadly restricting individual liberty, or low ε if read as protecting it — that story is authored separately with its own metrics), and civic_republican_reading (a third distinct constraint blending both). Each reading has its own beneficiary/victim structure and its own claimed_type; they are linked here rather than merged because measuring the same clause under different interpretive frameworks yields materially different ε values — the hallmark of a kernel requiring decomposition rather than a single constraint requiring an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
