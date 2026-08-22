% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__civic_republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__civic_republican_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: second_amendment_arms_right__civic_republican_reading
 *   human_readable: Armed Citizenship Right (Civic-Republican Reading)
 *   domain: constitutional_law/political_philosophy/legal_interpretation
 *
 * SUMMARY:
 *   A constitutional settlement secures the armed citizen body as a
 *   republican institution: citizens keep and bear arms in connection with
 *   militia service, owe training and readiness duties, and are governed by
 *   qualification standards that condition — but may not dissolve — the armed
 *   civic body. The arrangement solved a real founding-era problem (defense
 *   without professional soldiers) and carried real burdens on its own
 *   members; over two centuries its compulsory core dissolved, its function
 *   migrated to a professional military, and what remains is a protected
 *   status, a maturing qualification regime, and a large statutory-ceremonial
 *   residue. This story authors the civic-republican reading of the
 *   arms-provision kernel alone, with one stable epsilon; the
 *   individual-right and collective-right readings are separate constraint
 *   files linked through network.affects_constraints, and the decomposition
 *   rationale is recorded in the dual-formulation note and the kernel-context
 *   field. KEY AGENTS (by structural relationship): -
 *   citizen_militia_members: dual-position participant (moderate/constrained)
 *   — holds the protected status and owes the training, equipment, and
 *   readiness costs - federal_and_state_governments: agenda-setter and
 *   constrained party (institutional/trapped) — administers qualification and
 *   training, has surrendered the disarmament option -
 *   non_participating_public: primary cost-bearer outside membership
 *   (powerless/trapped) — lives under the arrangement's security conditions
 *   without its protections - firearms_industry: commercial beneficiary
 *   (powerful/arbitrage) — supplies the arms, training, and equipment the
 *   regime keeps in lawful demand - gun_rights_advocacy_organizations:
 *   organizational beneficiary and trainer (organized/mobile) — converts the
 *   provision's salience into membership, funds, and litigation capacity -
 *   professional_officer_corps: excluded rival institution
 *   (institutional/mobile) — the standing military the settlement was
 *   designed to make unnecessary - federal_courts: analytical observer
 *   (institutional/analytical) — adjudicates which understanding of the
 *   provision governs
 *
 * KEY AGENTS:
 *   - citizen_militia_members: dual-position participant (moderate/constrained) — right-holder and duty-bearer in one seat
 *   - federal_and_state_governments: agenda-setter and constrained party (institutional/trapped) — administers the regime, has surrendered the disarmament option
 *   - non_participating_public: primary cost-bearer outside membership (powerless/trapped) — bears the security environment without the protections
 *   - firearms_industry: commercial beneficiary (powerful/arbitrage) — captures the monetizable demand the regime sustains
 *   - gun_rights_advocacy_organizations: organizational beneficiary and trainer (organized/mobile) — converts the provision's salience into funds and litigation capacity
 *   - professional_officer_corps: excluded rival institution (institutional/mobile) — the standing military the settlement was built to avoid needing
 *   - federal_courts: analytical observer (institutional/analytical) — decides which understanding of the provision controls
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, 0.45).
domain_priors:suppression_score(second_amendment_arms_right__civic_republican_reading, 0.34).
domain_priors:theater_ratio(second_amendment_arms_right__civic_republican_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__civic_republican_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__civic_republican_reading, "Armed Citizenship Right (Civic-Republican Reading)").
narrative_ontology:topic_domain(second_amendment_arms_right__civic_republican_reading, "constitutional_law/political_philosophy/legal_interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__civic_republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__civic_republican_reading, '2ef0542c-3243-4e6d-b26f-869e50d24522').
narrative_ontology:cs_kernel_codification('2ef0542c-3243-4e6d-b26f-869e50d24522', fixed_text).
narrative_ontology:cs_authority_grounding('2ef0542c-3243-4e6d-b26f-869e50d24522', lineage).
narrative_ontology:cs_interpretation_layer_present('2ef0542c-3243-4e6d-b26f-869e50d24522').
narrative_ontology:cs_reading_relation('2ef0542c-3243-4e6d-b26f-869e50d24522', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('2ef0542c-3243-4e6d-b26f-869e50d24522', second_amendment_arms_right__collective_right_reading, influences).
narrative_ontology:cs_axiom('2ef0542c-3243-4e6d-b26f-869e50d24522', foundational, armed_citizenship_prerequisite_of_republican_liberty).
narrative_ontology:cs_axiom_status(armed_citizenship_prerequisite_of_republican_liberty, holdable).
narrative_ontology:cs_axiom_grounding('2ef0542c-3243-4e6d-b26f-869e50d24522', armed_citizenship_prerequisite_of_republican_liberty, instrumental).
narrative_ontology:cs_axiom('2ef0542c-3243-4e6d-b26f-869e50d24522', secondary, citizen_bearer_holds_right_and_duty_jointly).
narrative_ontology:cs_axiom_status(citizen_bearer_holds_right_and_duty_jointly, holdable).
narrative_ontology:cs_axiom_grounding('2ef0542c-3243-4e6d-b26f-869e50d24522', citizen_bearer_holds_right_and_duty_jointly, conventional).
narrative_ontology:cs_reference_frame('2ef0542c-3243-4e6d-b26f-869e50d24522', founding_civic_militia_settlement).
narrative_ontology:cs_drift_state('2ef0542c-3243-4e6d-b26f-869e50d24522', contemporary_individual_doctrine_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('2ef0542c-3243-4e6d-b26f-869e50d24522', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, citizen_militia_members).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, firearms_industry).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, non_participating_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, citizen_militia_members).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, federal_and_state_governments).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, republican_self_governance_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, anti_standing_army_principle).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, citizen_soldiery_ideal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a constitutionally secured place as the armed civic body: entitled to keep and bear arms in connection with militia service, and obligated to train, qualify, equip themselves, and answer when called. Their costs are paid in time, money, and personal risk; their returns are legal protection for arms possession, civic standing, and a share in collective defense. Leaving the arrangement means renouncing the protected status rather than simply declining the duty.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, citizen_militia_members, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, citizen_militia_members, payer).

% Set qualification standards, organize and fund training, and police the line between conditioning the armed civic body and infringing it. They cannot disarm the citizen body wholesale or dissolve the arrangement, and they have surrendered the option of holding all armed force in official hands. What they receive is a defense resource they did not have to build from scratch and a legitimacy claim rooted in shared civic burden.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, federal_and_state_governments, agenda_setter,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, federal_and_state_governments, payer).

% Live under the security conditions the arrangement produces without holding its protections or owing its duties. They bear the background risk of widespread arms and, where qualification regimes exclude them, are locked out of the civic role the arrangement centers. Relocation or legal change are the only exits available, and both are slow and expensive.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, non_participating_public, payer,
    powerless, biographical, trapped, national).

% Manufactures and sells the arms, ammunition, and training services the arrangement keeps in lawful demand; qualification and proficiency regimes channel customers toward courses, ranges, and compliant equipment. Its primary market is national but its production and sales networks span jurisdictions, so it can shift product lines and advocacy spending across borders if any single market closes.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, firearms_industry, beneficiary,
    powerful, biographical, arbitrage, global).

% Recruit members, raise funds, run marksmanship and safety training programs, and litigate around the arms provision. They administer much of the practical training apparatus the well-regulated language contemplates and convert the provision's continued salience into organizational revenue and political influence.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, gun_rights_advocacy_organizations, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, gun_rights_advocacy_organizations, agenda_setter).

% The standing professional military the founding arrangement was designed to make unnecessary. It operates outside the civic-militia bargain and holds no seat in its administration; its institutional preference, voiced from the founding era onward, has been for reliable professional forces rather than citizen levies. It encounters the arrangement chiefly as a constitutional rival for the defense function.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, professional_officer_corps, excluded,
    institutional, generational, mobile, national).

% Adjudicate which understanding of the arms provision governs, drawing on founding-era materials, militia statutes, and precedent. Their interpretations redistribute the arrangement's protections and obligations among the other seats; they themselves bear no arms duties and qualify for no membership.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__civic_republican_reading, firearms_industry).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__civic_republican_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a dispersed, pre-organized defensive capacity distributed across the citizenry so that a republic need not depend on professional soldiers for its security; ties military capability to citizenship itself and gives government a trained population it can call on without building and garrisoning a standing force.
% TRANSFER_FUNCTION: Moves training labor, equipment outlays, and readiness obligation from citizen-militia members into the collective defense pool; moves regulatory authority from ordinary legislative discretion into a bounded space conditioned by the civic-participation norm; moves commercial demand for arms and instruction toward suppliers and instructors.
% ABSENT_VOICES: Persons disqualified by fitness, character, or status gates — historically enslaved and freed Black Americans barred from militia rolls and women excluded from membership; today applicants denied under medical or misdemeanor criteria — and residents of communities that absorb the violence risk without sharing the civic membership. They stand outside the arrangement's membership definition, which is precisely why the terms of membership never had to answer to them.
% DISAPPEARANCE_RATIONALE: Federal and state firearms law would be rewritten within a legislative session or two; licensing regimes, preemption doctrines, and the litigation ecosystem organized around the provision would lose their anchor; the industry's domestic legal environment and the advocacy sector built on defending the provision would reorganize; and the question of who may hold arms would pass to ordinary legislative majorities in every state.
% FOUNDING_PROBLEM: How a republic secures itself without depending on professional soldiers, whom the founding generation regarded as instruments of tyranny: distribute arms, training, and organization among the citizenry so that the people themselves constitute the defensive resource.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the early republic and the standing-army literature corroborate both the problem's centrality at founding and its institutional eclipse; the post-1947 permanent defense establishment, accepted without serious republican objection, attests from outside the benefiting parties that the problem ceased to organize policy; the reading's own scholarly defenders attest continued liveness in principle. No seat inside the benefiting parties is the sole source of the genealogy.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__civic_republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__civic_republican_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__civic_republican_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_arms_right__civic_republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__civic_republican_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__civic_republican_reading_tests).
:- end_tests(second_amendment_arms_right__civic_republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.45 (moderate) because the arrangement's referent — the constitutional protection of armed citizenship with its attendant training and qualification regime — combines a genuine coordination achievement (dispersed defensive capacity, demonstrated functionally from the Revolution through the Civil War) with real, ongoing costs: compulsory participation burdens in the founding era, qualification gates and training mandates in the contemporary era, and risk externalities borne by non-participants throughout. Suppression is authored at 0.34 as a raw structural property — it is NOT scaled by power or scope; only extractiveness is scaled by the engine's directionality and scope modifiers. The 0.34 reflects legal-compulsion machinery (enrollment mandates, muster fines historically; permit, character, and proficiency gates today) at its current post-consolidation intensity. Theater_ratio 0.60 reflects the large ceremonial and statutory residue (unorganized-militia statutes with no operational content) relative to functioning qualification and training activity. Accessibility_collapse 0.40: alternatives remain live — the professional standing military realized the arrangement's principal rival, and a competing individual-liberty doctrine currently controls the text — so understanding this arrangement does not close off rival arrangements. Resistance 0.65: courts, legislatures, and social movements actively contest the arrangement's scope and persistence. Claim and metrics are authored independently: the claim is tangled_rope because both a coordination function and an extraction side are structurally present and active enforcement is required; the metrics report operating values, and the engine computes per-seat classifications from the structural data without reference to the claim. All three tracked metric series share one eight-point grid (1792, 1815, 1840, 1865, 1903, 1950, 1990, 2026) so no row is backfilled or grid-misaligned.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (federal_and_state_governments) and the payer seat (non_participating_public) should compute differently. From the government seat the arrangement is a self-imposed constitutional settlement: it surrenders the disarmament and force-monopoly options but buys defensive capacity, civic legitimacy, and a regulated rather than anarchic arms environment — a mixed, mostly beneficial position. From the non-participant seat the same structure delivers exposure without membership: the security environment is produced by others' rights and duties, and the qualification gates that define membership are gates the non-participant never consented to. The citizen_militia_members seat sits between: right and duty arrive jointly, so the seat prices its own participation. The engine computes these divergences from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low-directionality seats: firearms_industry and gun_rights_advocacy_organizations collect commercially and organizationally from the arrangement's operation (d near the beneficiary end). The victim declaration maps non_participating_public to a high-directionality seat: exposed to the arrangement's security environment, holding no membership, with trapped exit — the strongest amplification in the story. Citizen_militia_members are declared beneficiaries but carry the duty side as a secondary payer role: the derivation chain would read the beneficiary declaration and produce a strongly beneficiary-side d (roughly 0.15); the joint right-and-duty position warrants the declared override to 0.45, near symmetric, because training costs, equipment outlays, and personal risk offset the protected status. Federal_and_state_governments are left to structural derivation: as agenda-setters with a secondary payer position (the surrendered disarmament monopoly) their d lands mildly beneficiary-side, matching the settlement's negotiated character. No override is applied at the institutional level generally because it would collide with the observer and excluded seats, whose directionalities are not extraction-bearing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing a republic without a standing army by making the citizenry itself the defensive resource — was live and load-bearing from 1792 through the Civil War, then decayed as the professional military became politically uncontested. By 1903 the compulsory civic-military obligation was legislatively dissolved into the federally organized Guard, and the unorganized militia persisted thereafter as statutory fiction. The measurement series records the lifecycle: theater_ratio climbs 0.10 to 0.60 while suppression_requirement rises during the founding-era enforcement build-up (1792-1815), collapses as enforcement decays (1840-1950), then re-forms at lower amplitude as qualification gates mature (1990-2026). The classification prevents mislabeling in both directions: reading the arrangement as pure rope ignores the duty-side extraction, the exclusion history, and the risk externality on non-participants; reading it as pure snare misses the demonstrated coordination achievement and the fact that the heaviest historical burdens fell on the members themselves rather than on a captive outgroup. On mandate obsolescence: the founding mandate is institutionally dead though normatively contested, the arrangement persists on theatrical residue plus a transformed qualification function, and the civic-republican frame itself is under repudiation pressure from controlling doctrine — hence founding_problem_status contested, a high theater_ratio, and the drift toward inertial persistence visible in the series.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates the civic_republican_reading of the second_amendment_arms_right kernel. The sibling readings relocate the constraint''s foundation and bearer — individual_right_reading grounds the entitlement in pre-political individual liberty with no duty attached; collective_right_reading locates it in state militia institutions with no citizen-level claim outside organized service. Which reading controls the kernel''s legal operation, and what would each sibling structurally change?',
    'Sustained doctrinal adjudication: if controlling jurisprudence adopts one reading exclusively, the losing readings'' constraint files become counterfactual descriptions; scholarly convergence on a hybrid (individual source, civic scope) would dissolve the tripartite split into a single composite constraint.',
    'If individual_right_reading controls alone, this constraint''s duty-side costs and civic-qualification extraction vanish and its beneficiary set reduces to unrestricted possessors; if collective_right_reading controls, citizen_militia_members cease to be beneficiaries at all. The disagreement is located at the right''s foundation (civic-institutional versus pre-political versus state-institutional), not at its scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Kernel contest: three readings assign different foundations and bearers to one amendment text.').

omega_variable(
    residual_civic_function_under_standing_military,
    'Does armed citizenship still perform any republican-security function once a permanent professional military exists, or is the civic-military function fully absorbed into the standing establishment?',
    'Civil-military relations research and comparative study of guard and reservist systems: measure whether dispersed citizen capability contributes deterrence, continuity, or domestic-emergency capacity beyond the professional force.',
    'If the function is nil, the arrangement''s remaining activity is theatrical maintenance and the classification drifts toward inertial persistence; if partial (reserve integration, disaster response, legitimacy contribution), a real coordination core remains and the tangled_rope claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_civic_function_under_standing_military, empirical, 'Whether the civic-military coordination function survived the standing-army transition.').

omega_variable(
    exclusion_intrinsic_or_contingent,
    'Is the historical exclusionary operation of the civic frame — racialized and gendered militia membership defining who counted as the armed citizen body — intrinsic to civic-identity coordination, or contingent abuse correctable within the frame?',
    'Compare jurisdictions and periods with inclusive qualification regimes: if opening membership to all qualified adults preserves the frame''s function without reproducing the exclusion pattern, the exclusion was contingent.',
    'If intrinsic, the identity-coordination function is itself an extraction vehicle and measured extraction understates the harm to excluded populations; if contingent, the frame is reformable without dissolution and the victim set shrinks to the genuinely excluded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_intrinsic_or_contingent, conceptual, 'Identity-as-cover risk: whether the civic membership boundary necessarily excludes.').

omega_variable(
    duty_side_enforcement_trajectory,
    'Will qualification and training enforcement continue maturing (permit regimes hardening, training mandates spreading), or relax as litigation narrows gate criteria?',
    'Track permit-denial rates, training-mandate legislation, and appellate treatment of qualification burdens over the coming decade.',
    'Continued maturation raises the payer-seat burden and pushes the arrangement toward harder enforcement with rising effective extraction on non-participants; relaxation returns it toward low-suppression coordination with the duty side priced down.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(duty_side_enforcement_trajectory, empirical, 'Direction of the qualification-gate enforcement ratchet.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__civic_republican_reading, 1792, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_civic_republican_tr_t1792, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1792, 0.1).
narrative_ontology:measurement_basis(sa_civic_republican_tr_t1792, observed).
narrative_ontology:measurement(sa_civic_republican_tr_t1815, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1815, 0.2).
narrative_ontology:measurement_basis(sa_civic_republican_tr_t1815, observed).
narrative_ontology:measurement(sa_civic_republican_tr_t1840, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1840, 0.55).
narrative_ontology:measurement_basis(sa_civic_republican_tr_t1840, observed).
narrative_ontology:measurement(sa_civic_republican_tr_t1865, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1865, 0.45).
narrative_ontology:measurement_basis(sa_civic_republican_tr_t1865, observed).
narrative_ontology:measurement(sa_civic_republican_tr_t1903, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1903, 0.5).
narrative_ontology:measurement_basis(sa_civic_republican_tr_t1903, observed).
narrative_ontology:measurement(sa_civic_republican_tr_t1950, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1950, 0.7).
narrative_ontology:measurement_basis(sa_civic_republican_tr_t1950, observed).
narrative_ontology:measurement(sa_civic_republican_tr_t1990, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1990, 0.62).
narrative_ontology:measurement_basis(sa_civic_republican_tr_t1990, observed).
narrative_ontology:measurement(sa_civic_republican_tr_t2026, second_amendment_arms_right__civic_republican_reading, theater_ratio, 2026, 0.6).
narrative_ontology:measurement_basis(sa_civic_republican_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(sa_civic_republican_be_t1792, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1792, 0.52).
narrative_ontology:measurement_basis(sa_civic_republican_be_t1792, observed).
narrative_ontology:measurement(sa_civic_republican_be_t1815, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1815, 0.54).
narrative_ontology:measurement_basis(sa_civic_republican_be_t1815, observed).
narrative_ontology:measurement(sa_civic_republican_be_t1840, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1840, 0.58).
narrative_ontology:measurement_basis(sa_civic_republican_be_t1840, observed).
narrative_ontology:measurement(sa_civic_republican_be_t1865, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1865, 0.45).
narrative_ontology:measurement_basis(sa_civic_republican_be_t1865, observed).
narrative_ontology:measurement(sa_civic_republican_be_t1903, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1903, 0.38).
narrative_ontology:measurement_basis(sa_civic_republican_be_t1903, observed).
narrative_ontology:measurement(sa_civic_republican_be_t1950, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement_basis(sa_civic_republican_be_t1950, observed).
narrative_ontology:measurement(sa_civic_republican_be_t1990, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement_basis(sa_civic_republican_be_t1990, observed).
narrative_ontology:measurement(sa_civic_republican_be_t2026, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 2026, 0.45).
narrative_ontology:measurement_basis(sa_civic_republican_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(sa_civic_republican_su_t1792, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1792, 0.55).
narrative_ontology:measurement_basis(sa_civic_republican_su_t1792, observed).
narrative_ontology:measurement(sa_civic_republican_su_t1815, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1815, 0.6).
narrative_ontology:measurement_basis(sa_civic_republican_su_t1815, observed).
narrative_ontology:measurement(sa_civic_republican_su_t1840, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1840, 0.35).
narrative_ontology:measurement_basis(sa_civic_republican_su_t1840, observed).
narrative_ontology:measurement(sa_civic_republican_su_t1865, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1865, 0.3).
narrative_ontology:measurement_basis(sa_civic_republican_su_t1865, observed).
narrative_ontology:measurement(sa_civic_republican_su_t1903, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1903, 0.15).
narrative_ontology:measurement_basis(sa_civic_republican_su_t1903, observed).
narrative_ontology:measurement(sa_civic_republican_su_t1950, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1950, 0.12).
narrative_ontology:measurement_basis(sa_civic_republican_su_t1950, observed).
narrative_ontology:measurement(sa_civic_republican_su_t1990, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement_basis(sa_civic_republican_su_t1990, observed).
narrative_ontology:measurement(sa_civic_republican_su_t2026, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 2026, 0.34).
narrative_ontology:measurement_basis(sa_civic_republican_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__civic_republican_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__collective_right_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition: the colloquial label 'the Second Amendment right' covers three structurally distinct claims with different epsilons, beneficiary sets, and victim sets. This file authors the civic-republican member of the family (armed citizenship, right and duty joined, civic-norm-bounded regulation). The individual-right sibling relocates the foundation to pre-political liberty and deletes the duty side; the collective-right sibling relocates the bearer to state militia institutions and denies the citizen-level claim. The founding civic settlement is the historical upstream of both siblings — each cites the militia clause — so this reading exerts structural pressure on the collective reading's legitimacy conditions (civic scholarship drained the collective reading's distinctiveness by splitting the militia premise from the state-centered conclusion) while coexisting with the individual reading as live positions held by different factions. Each story links the other two through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_arms_right__civic_republican_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
