% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__prohibition_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: substance_control_legitimacy__prohibition_reading
 *   human_readable: Prohibition Criminalization of Substance Use
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The prohibition_reading of substance_control_legitimacy asserts that
 *   substance use is inherently harmful and that the state derives moral
 *   authority from criminalizing use to prevent harm. This reading
 *   instantiates a constraint where criminal law targets users directly,
 *   generating a carceral extraction apparatus (police, courts, prisons) and
 *   a prohibited market whose violence is externalized onto users and
 *   targeted communities. The stated coordination function — preventing
 *   substance harm — is structurally overridden by the extraction function:
 *   enforcement resources flow to arrest and incarceration rather than
 *   treatment; harm metrics worsen under prohibition; institutional
 *   beneficiaries (law enforcement, prison contractors, moral advocacy
 *   groups) capture rents from the arrangement. The constraint is claimed as
 *   a moral duty (mountain-like framing) but operates as a snare: high
 *   extraction from criminalized users, active suppression of alternatives
 *   (harm reduction, legalization), and no sunset mechanism. The ε-invariance
 *   principle requires this reading to be a separate constraint from
 *   harm_reduction_reading and legalization_reading, each with its own ε,
 *   victim set, and classification.
 *
 * KEY AGENTS:
 *   - law_enforcement_agencies: Primary beneficiary (institutional/powerful/arbitrage) — controls enforcement resources, captures funding and mission expansion from prohibition
 *   - prison_industrial_complex: Primary beneficiary (institutional/powerful/arbitrage) — receives incarcerated bodies as revenue stream, lobbies for sentence enhancements
 *   - moral_regulation_advocacy_organizations: Secondary beneficiary (organized/moderate/mobile) — gains political capital and funding from framing prohibition as moral imperative
 *   - people_who_use_drugs: Primary victim (powerless/trapped/local) — bears arrest, incarceration, overdose risk from unregulated supply, criminal record collateral consequences
 *   - communities_targeted_by_enforcement: Primary victim (powerless/identity_locked/regional) — experiences concentrated policing, family separation, economic destabilization; exit blocked by structural racism and geographic segregation
 *   - families_of_incarcerated_individuals: Secondary victim (moderate/constrained/national) — bears financial, emotional, and caregiving costs of incarceration
 *   - public_health_authorities: Excluded observer (institutional/analytical/national) — advocates harm reduction but is structurally excluded from drug policy decisions by criminalization framework
 *   - elected_officials: Agenda setter (institutional/powerful/arbitrage) — enacts and maintains prohibition laws; responds to moral advocacy and law enforcement lobbying
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, 0.78).
domain_priors:suppression_score(substance_control_legitimacy__prohibition_reading, 0.88).
domain_priors:theater_ratio(substance_control_legitimacy__prohibition_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_legitimacy__prohibition_reading, "Prohibition Criminalization of Substance Use").
narrative_ontology:topic_domain(substance_control_legitimacy__prohibition_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__prohibition_reading, '62fc817c-e7af-44f4-be51-caa66390f373').
narrative_ontology:cs_kernel_codification('62fc817c-e7af-44f4-be51-caa66390f373', implicit).
narrative_ontology:cs_authority_grounding('62fc817c-e7af-44f4-be51-caa66390f373', extraction).
narrative_ontology:cs_interpretation_layer_present('62fc817c-e7af-44f4-be51-caa66390f373').
narrative_ontology:cs_reading_relation('62fc817c-e7af-44f4-be51-caa66390f373', substance_control_legitimacy__harm_reduction_reading, forecloses).
narrative_ontology:cs_reading_relation('62fc817c-e7af-44f4-be51-caa66390f373', substance_control_legitimacy__legalization_reading, influences).
narrative_ontology:cs_axiom('62fc817c-e7af-44f4-be51-caa66390f373', foundational, substance_use_is_inherently_harmful).
narrative_ontology:cs_axiom_status(substance_use_is_inherently_harmful, holdable).
narrative_ontology:cs_axiom_grounding('62fc817c-e7af-44f4-be51-caa66390f373', substance_use_is_inherently_harmful, deontological).
narrative_ontology:cs_axiom('62fc817c-e7af-44f4-be51-caa66390f373', foundational, state_has_moral_duty_to_prevent_self_harm_through_criminalization).
narrative_ontology:cs_axiom_status(state_has_moral_duty_to_prevent_self_harm_through_criminalization, holdable).
narrative_ontology:cs_axiom_grounding('62fc817c-e7af-44f4-be51-caa66390f373', state_has_moral_duty_to_prevent_self_harm_through_criminalization, deontological).
narrative_ontology:cs_reference_frame('62fc817c-e7af-44f4-be51-caa66390f373', moral_order_criminalization_framework).
narrative_ontology:cs_drift_state('62fc817c-e7af-44f4-be51-caa66390f373', contemporary_overdose_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('62fc817c-e7af-44f4-be51-caa66390f373', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, prison_industrial_complex).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, moral_regulation_advocacy_organizations).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, communities_targeted_by_enforcement).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, families_of_incarcerated_individuals).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__prohibition_reading, substance_use_is_inherently_harmful).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__prohibition_reading, state_moral_duty_to_prevent_self_harm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive billions in dedicated enforcement funding (Byrne grants, asset forfeiture, federal drug war budgets); control arrest priorities and resource allocation; gain institutional legitimacy and mission scope from prohibition. Exit is arbitrage: they can pivot enforcement to other crimes but lose the dedicated funding stream and political capital.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies, beneficiary,
    institutional, generational, arbitrage, national).

% Private prison contractors, prison labor beneficiaries, and correctional officer unions capture per-diem payments, construction contracts, and below-market labor from incarcerated populations. Drug offenses provide a steady inflow of bodies. Exit is arbitrage: they can adapt to other populations but lose the scale and predictability of drug-war incarceration.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, prison_industrial_complex, beneficiary,
    institutional, generational, arbitrage, national).

% Faith-based and secular moral advocacy groups gain political influence, fundraising base, and media access by championing prohibition as moral duty. They lobby for harsher penalties and against harm reduction. Exit is mobile: they can shift issue focus but lose the mobilization power of 'protecting children/families from drugs.'
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, moral_regulation_advocacy_organizations, beneficiary,
    organized, biographical, mobile, national).

% Bear the full weight of criminalization: arrest, incarceration, overdose from unregulated supply, criminal records blocking housing/employment/education, stigma in healthcare. Cannot exit the 'user' identity without cessation (which the constraint makes harder by blocking treatment) — identity_locked in practice, but structurally trapped by the criminalization itself. The constraint creates the victim class it then extracts from.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, people_who_use_drugs, payer,
    powerless, immediate, trapped, local).

% Predominantly Black, Brown, and Indigenous communities experience concentrated policing, mass incarceration, family separation, and economic extraction (fines, fees, asset forfeiture). Geographic and structural segregation makes exit impossible — the constraint follows them. Identity_locked: their community identity is constituted through the shared experience of targeted enforcement; leaving the community does not remove the structural targeting.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, communities_targeted_by_enforcement, payer,
    powerless, biographical, identity_locked, regional).

% Absorb the costs of incarceration: lost income, phone/commissary expenses, travel for visits, caregiving for children left behind, emotional trauma. Excluded from policy decisions about sentencing and alternatives. Exit is constrained: they can advocate for reform but face the same political barriers as other reform movements.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, families_of_incarcerated_individuals, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__prohibition_reading, families_of_incarcerated_individuals, excluded).

% Hold the evidence base for harm reduction (syringe programs, OAT, safe consumption sites) but are structurally excluded from drug policy by the criminalization framework. Their recommendations are treated as advisory at best, overridden by law enforcement priorities. Exit is analytical: they observe the failure but cannot change the constraint from their seat.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, public_health_authorities, excluded,
    institutional, generational, analytical, national).

% Enact and maintain prohibition laws; control appropriations for enforcement vs. treatment; respond to moral advocacy and law enforcement lobbying. Can change the law (arbitrage exit) but face primary challenges and 'soft on crime' attacks from beneficiaries of the current arrangement. Their re-election depends on maintaining the constraint's political coalition.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, elected_officials, agenda_setter,
    institutional, biographical, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stated coordination function is preventing substance-related harm through deterrence and supply reduction. The actual coordination function (what the arrangement reliably produces) is the coordination of carceral resource flows: it aligns police, prosecutors, prisons, and moral advocates around a shared extraction target (criminalized users) and a shared legitimizing narrative (moral duty).
% TRANSFER_FUNCTION: Moves liberty, labor, and public funds from criminalized populations and their communities to law enforcement agencies, prison operators, and moral advocacy organizations. The transfer is enforced through arrest, incarceration, fines, fees, asset forfeiture, and the threat of violence. Black market profits are a parallel transfer to criminal organizations, enabled by the prohibition regime.
% ABSENT_VOICES: People who use drugs (especially those not in recovery), communities most targeted by enforcement, and public health practitioners are structurally excluded from the policy conversation. They are present as objects of policy but absent as authors of it. Their exclusion is maintained by the criminalization itself — a felony conviction removes voting rights; stigma silences users; law enforcement controls the narrative.
% DISAPPEARANCE_RATIONALE: If prohibition vanished overnight, the carceral extraction apparatus would lose its primary justification and target population; police and prison budgets would contract; black markets would collapse as legal supply replaces them; overdose deaths would drop with regulated supply; communities targeted by enforcement would experience immediate decarceration. The world would rearrange fundamentally — the constraint is load-bearing for the carceral state.
% FOUNDING_PROBLEM: The prohibition_reading was built to solve the perceived problem of widespread substance use seen as moral degeneracy and social threat (early 20th century temperance, 1970s 'war on drugs' framing). The founding problem was framed as: 'How does the state fulfill its moral duty to protect citizens from the inherent harm of substance use?'
% FOUNDING_PROBLEM_CORROBORATION: Prohibition advocates (DEA, ONDCP, moral advocacy groups) attest the problem is live and worsening (fentanyl crisis). Harm reduction advocates, public health researchers, and affected communities attest the problem is misdiagnosed — prohibition creates the harms it claims to prevent (overdose, violence, disease). Independent historical analysis (e.g., Courtwright, Musto, Alexander) corroborates that the founding problem was politically constructed to enable carceral expansion and racial control, not discovered as a natural fact.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(substance_control_legitimacy__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__prohibition_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness 0.78 reflects the massive resource transfer from criminalized populations to carceral institutions: $47B+ annual US enforcement spending, 1.5M+ annual drug arrests, millions incarcerated. Suppression 0.88 captures the active elimination of alternatives — harm reduction programs blocked, research suppressed, legalization politically punished. Theater ratio 0.28 indicates some genuine enforcement activity (interdiction, arrests) but a growing share of activity is performative (drug war rhetoric, low-level possession arrests that don't reduce supply). Accessibility collapse 0.42: alternatives exist (Portugal model, legal cannabis markets) but are politically suppressed, not naturally impossible. Resistance 0.67: sustained reform movements, state-level legalization, harm reduction advocacy — but resistance is fragmented and met with federal preemption. The claimed type (snare) diverges from the moral mountain framing; the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter/beneficiary seats (law enforcement, moral advocates, elected officials), the constraint appears as necessary moral order — a coordination mechanism protecting society from harm. From the payer/victim seats (users, targeted communities), the same structure operates as violent extraction: arrest, incarceration, family separation, market violence. The engine computes this divergence from the structural power/exit asymmetry. The excluded public health seat sees a failed public health policy; the observer seat sees a political economy trap. No single seat sees the full structure — the constraint's persistence depends on this perspectival fragmentation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (law enforcement, prison industry, moral advocates) have high power (institutional/powerful), generational horizons, arbitrage/mobile exit — they shape the constraint and capture its rents. Victims (people who use drugs, targeted communities) have low power (powerless), immediate/biographical horizons, trapped/identity_locked exit — they cannot exit the criminalization without ceasing to be the target population (identity_locked: criminal status and addiction stigma fuse to identity). Families occupy a constrained/moderate position. Public health authorities are excluded observers with analytical exit. Elected officials are agenda setters with institutional power and arbitrage exit (they can change the law but face electoral pressure from moral advocates and law enforcement). The beneficiary/victim declarations map directly to the carceral political economy: the constraint exists to transfer resources from criminalized populations to enforcement institutions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing substance harm) is contested and arguably dead — prohibition correlates with increased harm (overdose, violence, disease). The arrangement persists because its actual function (carceral extraction, institutional rent, moral signaling) is disconnected from its stated function. The mandate has atrophied into a snare: the coordination story is cover; persistence depends on suppressing alternatives and maintaining the victim class. This is not a piton (no residual coordination function) nor a tangled rope (no genuine coordination remaining). The snare classification prevents mislabeling this as 'well-intentioned but failed policy' — the extraction is structural, not accidental.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_prohibition_reading,
    'How does the prohibition_reading of the substance_control_legitimacy kernel structurally differ from harm_reduction_reading and legalization_reading?',
    'Compare victim sets, extraction pathways, and enforcement mechanisms across readings; the prohibition_reading uniquely criminalizes users, creating carceral extraction and black market violence externalities absent in other readings.',
    'If the structural delta is confirmed, the three readings are distinct constraints with different ε values and types — the kernel label masks three separate arrangements, consistent with ε-invariance decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_prohibition_reading, conceptual, 'This constraint is the prohibition_reading of kernel substance_control_legitimacy; sibling readings are harm_reduction_reading and legalization_reading. The prohibition_reading''s structural delta: users become victims via criminalization, high carceral extractiveness, black market violence externality.').

omega_variable(
    moral_duty_vs_extraction_boundary,
    'Is the stated moral duty to prevent harm the genuine motive, or a legitimizing cover for carceral extraction and institutional rent-seeking?',
    'Historical analysis of enforcement resource allocation vs. harm reduction outcomes; compare stated goals with actual expenditure patterns and institutional beneficiaries.',
    'If cover-story, the constraint is a snare with moral framing as extraction mechanism; if genuine but failed, it is a failed scaffold with high theater. Either way, the moral claim does not reduce extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_duty_vs_extraction_boundary, conceptual, 'Whether the moral-duty justification tracks the constraint''s actual function or masks extraction.').

omega_variable(
    black_market_violence_causality,
    'Is black market violence an intended feature of the prohibition regime (deterrence through chaos) or an unintended but tolerated externality?',
    'Analyze policy internal communications, enforcement prioritization, and historical responses to violence spikes; do authorities act to reduce violence or manage it as a deterrent signal?',
    'If intended, suppression includes violence-as-tool, raising extractiveness; if tolerated, suppression is enforcement-only and violence is an unpriced externality borne by victims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(black_market_violence_causality, empirical, 'Causal role of black market violence in the prohibition mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__prohibition_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1970, substance_control_legitimacy__prohibition_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(subs_tr_t1980, substance_control_legitimacy__prohibition_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(subs_tr_t1990, substance_control_legitimacy__prohibition_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(subs_tr_t2000, substance_control_legitimacy__prohibition_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(subs_tr_t2010, substance_control_legitimacy__prohibition_reading, theater_ratio, 2010, 0.27).
narrative_ontology:measurement(subs_tr_t2024, substance_control_legitimacy__prohibition_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(subs_be_t1970, substance_control_legitimacy__prohibition_reading, base_extractiveness, 1970, 0.52).
narrative_ontology:measurement(subs_be_t1980, substance_control_legitimacy__prohibition_reading, base_extractiveness, 1980, 0.61).
narrative_ontology:measurement(subs_be_t1990, substance_control_legitimacy__prohibition_reading, base_extractiveness, 1990, 0.72).
narrative_ontology:measurement(subs_be_t2000, substance_control_legitimacy__prohibition_reading, base_extractiveness, 2000, 0.76).
narrative_ontology:measurement(subs_be_t2010, substance_control_legitimacy__prohibition_reading, base_extractiveness, 2010, 0.78).
narrative_ontology:measurement(subs_be_t2024, substance_control_legitimacy__prohibition_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1970, substance_control_legitimacy__prohibition_reading, suppression_requirement, 1970, 0.72).
narrative_ontology:measurement(subs_su_t1980, substance_control_legitimacy__prohibition_reading, suppression_requirement, 1980, 0.78).
narrative_ontology:measurement(subs_su_t1990, substance_control_legitimacy__prohibition_reading, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(subs_su_t2000, substance_control_legitimacy__prohibition_reading, suppression_requirement, 2000, 0.87).
narrative_ontology:measurement(subs_su_t2010, substance_control_legitimacy__prohibition_reading, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(subs_su_t2024, substance_control_legitimacy__prohibition_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_legitimacy__prohibition_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__legalization_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, mass_incarceration_regime).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, civil_asset_forfeiture_system).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, police_militarization_program).

% DUAL FORMULATION NOTE:
% The substance_control_legitimacy kernel decomposes into three constraint stories: prohibition_reading (this story, snare), harm_reduction_reading (tangled_rope — genuine coordination function with residual extraction from institutional resistance), legalization_reading (rope — coordination via regulated markets). The prohibition_reading structurally influences the other two: its enforcement apparatus creates the institutional and legal barriers that harm_reduction must navigate and legalization must dismantle. The black market violence externality is a network effect exported to adjacent constraints (organized crime, community destabilization).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_legitimacy__prohibition_reading, institutional, 0.15).
constraint_indexing:directionality_override(substance_control_legitimacy__prohibition_reading, powerless, 0.95).
constraint_indexing:directionality_override(substance_control_legitimacy__prohibition_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
