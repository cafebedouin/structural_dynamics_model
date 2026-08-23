% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__revisionist_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__revisionist_zionism_reading, []).

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
 *   constraint_id: jewish_territorial_claim__revisionist_zionism_reading
 *   human_readable: Revisionist Zionist Maximalist Territorial Claim (Iron Wall Doctrine)
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   The revisionist Zionist reading of the Jewish territorial claim kernel,
 *   articulated in Jabotinsky's 1923 'Iron Wall' essay, asserts that Jewish
 *   sovereignty over both banks of the Jordan River is a non-negotiable right
 *   that can only be realized through overwhelming military force. Arab
 *   consent is explicitly rejected as a prerequisite — 'Zionist colonization
 *   must either stop, or else proceed regardless of the native population.'
 *   This constraint operated as the ideological engine of Revisionist
 *   paramilitaries (Irgun, Lehi), shaped Herut/Likud policy, and after 1967
 *   became the settlement movement's doctrinal core. The claim/metric gap is
 *   deliberate: the revisionists FRAME this as a rope (necessary coordination
 *   for Jewish survival) while the authored metrics describe a constraint
 *   that extracts land, sovereignty, and rights from the Palestinian Arab
 *   population through active military suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, 0.88).
domain_priors:suppression_score(jewish_territorial_claim__revisionist_zionism_reading, 0.92).
domain_priors:theater_ratio(jewish_territorial_claim__revisionist_zionism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__revisionist_zionism_reading, rope).
narrative_ontology:human_readable(jewish_territorial_claim__revisionist_zionism_reading, "Revisionist Zionist Maximalist Territorial Claim (Iron Wall Doctrine)").
narrative_ontology:topic_domain(jewish_territorial_claim__revisionist_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__revisionist_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__revisionist_zionism_reading, 'ff67cbd1-e1fe-4c17-abd4-984dd0c2c414').
narrative_ontology:cs_kernel_codification('ff67cbd1-e1fe-4c17-abd4-984dd0c2c414', fixed_text).
narrative_ontology:cs_authority_grounding('ff67cbd1-e1fe-4c17-abd4-984dd0c2c414', extraction).
narrative_ontology:cs_interpretation_layer_present('ff67cbd1-e1fe-4c17-abd4-984dd0c2c414').
narrative_ontology:cs_reading_relation('ff67cbd1-e1fe-4c17-abd4-984dd0c2c414', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff67cbd1-e1fe-4c17-abd4-984dd0c2c414', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff67cbd1-e1fe-4c17-abd4-984dd0c2c414', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('ff67cbd1-e1fe-4c17-abd4-984dd0c2c414', foundational, iron_wall_doctrine).
narrative_ontology:cs_axiom_status(iron_wall_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('ff67cbd1-e1fe-4c17-abd4-984dd0c2c414', iron_wall_doctrine, empirically_contingent).
narrative_ontology:cs_axiom('ff67cbd1-e1fe-4c17-abd4-984dd0c2c414', foundational, territorial_maximalism_non_negotiable).
narrative_ontology:cs_axiom_status(territorial_maximalism_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('ff67cbd1-e1fe-4c17-abd4-984dd0c2c414', territorial_maximalism_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('ff67cbd1-e1fe-4c17-abd4-984dd0c2c414', biblical_historic_claim_both_banks).
narrative_ontology:cs_drift_state('ff67cbd1-e1fe-4c17-abd4-984dd0c2c414', post_1967_occupation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ff67cbd1-e1fe-4c17-abd4-984dd0c2c414', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_leadership).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, jewish_settlers_revisionist).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, future_jewish_state_institutions).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arabs_west_bank).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arabs_east_bank).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, arab_neighboring_states).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__revisionist_zionism_reading, jewish_historic_right_to_entire_land).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__revisionist_zionism_reading, iron_wall_doctrine_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Formulates and enforces the Iron Wall doctrine through paramilitary organizations (Betar, Irgun, Lehi) and political parties (Hatzohar, Herut). Jabotinsky and successors define the maximalist territorial claim as non-negotiable and build military capacity to impose it. They control the ideological framing and direct the violent enforcement.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_leadership, agenda_setter,
    organized, generational, constrained, regional).

% Establish settlements across both banks of the Jordan River under revisionist auspices. Gain land, housing, and ideological fulfillment from the maximalist project. Their presence creates facts on the ground that make territorial compromise harder. Exit means abandoning homes and ideological commitment.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, jewish_settlers_revisionist, beneficiary,
    moderate, biographical, constrained, local).

% Inherit the territorial maximalist framework after 1948. The IDF, state bureaucracy, and settlement apparatus operationalize the Iron Wall doctrine. They benefit from the expanded territorial control and security doctrine but face international legitimacy costs. Can pivot to diplomatic frameworks (Camp David, Oslo) but the maximalist constraint persists in settlement policy.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, future_jewish_state_institutions, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__revisionist_zionism_reading, future_jewish_state_institutions, agenda_setter).

% Subject to displacement, military occupation, land expropriation, and denial of political rights under the maximalist claim. The Iron Wall doctrine explicitly denies their consent as relevant. No meaningful exit: surrounded by Israeli military control, Jordanian border closure (post-1948), and lack of international protection. Bear the full extractive cost of the constraint.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arabs_west_bank, payer,
    powerless, generational, trapped, local).

% Transjordanian/Palestinian population east of the Jordan River. Revisionist maximalism explicitly claims their territory as part of the Jewish state. Subject to refugee flows (1948, 1967), demographic pressure, and Hashemite regime's strategic dependence on Israel/US. The constraint treats their land as Jewish by right; their consent is irrelevant.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arabs_east_bank, payer,
    powerless, generational, trapped, local).

% Transjordan/Jordan, Syria, Lebanon, Egypt bear military confrontation costs, refugee burdens, and territorial loss from the maximalist project. The Iron Wall doctrine aims to compel their acceptance through repeated military defeat. They can resist militarily or diplomatically but face asymmetric power. Some eventually sign peace treaties (Egypt 1979, Jordan 1994) — partial exit at high cost.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, arab_neighboring_states, payer,
    moderate, biographical, constrained, regional).

% The colonial administration (1923-1948) that the revisionists directly challenged. The Iron Wall was formulated against British attempts to limit Jewish immigration/land purchase. British policy oscillated between repression and concession. Ultimately withdrew in 1948 — their exclusion from the revisionist framework was structural: the constraint required their removal.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, british_mandate_authorities, excluded,
    institutional, immediate, arbitrage, regional).

% Labor Zionist (Ben-Gurion, Mapai) and General Zionist leadership who accepted partition (1937 Peel, 1947 UN) and built the Yishuv's institutional core. They competed with revisionists for hegemony but eventually incorporated revisionist paramilitaries into IDF. Observed the constraint from a rival position — their pragmatic territorial compromise was the main internal alternative.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, mainstream_zionist_leadership, observer,
    organized, generational, mobile, national).

% Emergent Palestinian political leadership (Husseini, Nashashibi, later PLO) that the Iron Wall doctrine explicitly rendered irrelevant — 'no voluntary agreement possible.' Their resistance (1936-39 revolt, 1948 war, fedayeen, intifadas) is the constraint's primary friction. Excluded from any negotiation framework by design.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, palestinian_national_movement, excluded,
    organized, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a dispersed Jewish population into a sovereign territorial entity across both banks of the Jordan River through unified military-political effort, solving the collective action problem of state-building under hostile conditions by treating Arab opposition as a fixed variable to be overcome by force rather than negotiated.
% TRANSFER_FUNCTION: Moves land, water resources, demographic control, and sovereign authority from the indigenous Arab population (both banks) to the Jewish settler population via military conquest, legal expropriation, and settlement expansion. The transfer is unidirectional and non-consensual by design.
% ABSENT_VOICES: The Palestinian Arab population (both banks) — their consent is explicitly declared irrelevant by the Iron Wall doctrine. Transjordanian civilians whose territory is claimed. Mainstream Zionist factions who favored partition and negotiation — their pragmatic alternative was marginalized by revisionist maximalism. Arab neighboring states whose acceptance is sought only through military compulsion.
% DISAPPEARANCE_RATIONALE: If the Iron Wall doctrine and maximalist claim vanished overnight, the ideological justification for settlements beyond the Green Line, the refusal of Palestinian statehood, the 'Jordan is Palestine' demographic transfer proposals, and the permanent occupation framework would collapse. The Israeli-Palestinian conflict would restructure around 1967 lines or binational alternatives. The revisionist movement's entire political reason for being would dissolve.
% FOUNDING_PROBLEM: The perceived impossibility of achieving Jewish statehood through Arab consent — Jabotinsky's diagnosis that 'there can be no voluntary agreement between us and the Palestine Arabs' because Arab nationalism would never accept Jewish sovereignty in any form. The 'Arab question' as existential barrier to Zionism.
% FOUNDING_PROBLEM_CORROBORATION: Revisionist ideologues (Jabotinsky, Begin, Shamir) attest the problem remains live — no genuine Arab acceptance exists, only ceasefires. Mainstream Zionist/Israeli leaders (Ben-Gurion, Rabin, Peres) attested it was solvable through partition and peace treaties (Egypt, Jordan, Oslo). Palestinian historians (Khalidi, Pappé) and Arab leaders attest the problem was constructed by Zionist refusal to recognize Palestinian rights. Independent historians (Morris, Segev, Shlaim) document the ideological formation and its consequences from archival sources outside the benefiting parties.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__revisionist_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__revisionist_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__revisionist_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_territorial_claim__revisionist_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__revisionist_zionism_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88 by 1967) because the constraint transfers virtually all land, water, and political authority from the indigenous population to the settler population with zero compensation or consent mechanism. Suppression is extreme (0.92) because the constraint's persistence depends entirely on military occupation, legal exclusion, and the denial of Palestinian political agency — not on participant preference. Theater ratio is low-moderate (0.28) because the military enforcement is genuinely functional for the constraint's aims (it works), though diplomatic 'peace process' rituals add performative layer after 1967. The measurement series runs on a shared time grid (1923-1967) capturing the doctrinal formation, paramilitary phase, statehood, and territorial maximalism's fulfillment in 1967.
 *
 * PERSPECTIVAL GAP:
 *   The revisionist seat computes this as rope (coordination for survival against existential threat). The Palestinian payer seats compute it as snare (pure extraction enforced by arms). The future state institution seat computes it as tangled_rope (coordination function — state-building — fused with extraction — occupation). The engine computes this divergence from the structural data: same constraint, different directionalities, different effective extractions.
 *
 * DIRECTIONALITY LOGIC:
 *   Revisionist leadership (agenda_setter) sits at d ≈ 0.1 — they formulate and benefit from the constraint. Jewish settlers (beneficiary) at d ≈ 0.25 — gain land/ideology but bear some risk. Future state institutions (beneficiary/agenda_setter) at d ≈ 0.15 — inherit and operationalize. Palestinian Arabs both banks (payers) at d ≈ 0.95 — trapped, zero consent, total extraction. Arab states (payers) at d ≈ 0.7 — constrained but with some diplomatic exit. British (excluded) at d ≈ 0.0 — colonial power removed. Mainstream Zionists (observer) at d ≈ 0.4 — pragmatic rivals who eventually incorporated revisionist personnel but not maximalist doctrine. Palestinian national movement (excluded) at d ≈ 0.95 — structurally rendered irrelevant by design.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Arab rejection of Jewish sovereignty) was declared 'live' by revisionists in 1923. By 1979 (Egypt peace), 1994 (Jordan peace), 1993 (Oslo), Arab acceptance of Israeli existence was demonstrated — but revisionists redefine 'acceptance' as 'surrender to maximalist terms.' The mandate has atrophied into a self-justifying extraction machine: the constraint persists because the institutions it built (settlements, IDF doctrine, legal framework) now generate their own inertia. No beneficiary profits enough to maintain it voluntarily; no victim can dismantle it. Classic mandatrophy: the problem the arrangement was built for has transformed, but the arrangement intensifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_identity,
    'This constraint is one reading (revisionist_zionism_reading) of the contested kernel jewish_territorial_claim. What would the sibling readings (political_zionism_reading, labor_zionism_reading, cultural_zionism_reading) change structurally in the constraint''s ε, beneficiary/victim structure, and classification?',
    'Author separate constraint stories for each sibling reading with their own ε, stakeholders, and metrics. Compare the structural deltas: political_zionism would lower extractiveness (partition acceptance), labor_zionism would shift coordination function to socialist transformation, cultural_zionism would eliminate territorial sovereignty claim entirely.',
    'If sibling readings produce substantially different ε values and classifications, this confirms the kernel decomposes into multiple constraints per ε-invariance principle. The revisionist reading''s high extraction/suppression is not a property of ''the Jewish territorial claim'' simpliciter but of THIS reading''s specific structural commitments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Kernel/reading decomposition and structural delta across sibling readings').

omega_variable(
    natural_right_vs_constructed_claim,
    'Is the maximalist territorial claim (both banks of Jordan) a genuine natural/historic right that emerges from Jewish history (mountain framing), or a constructed ideological claim that serves revisionist movement interests (snare/tangled_rope framing)?',
    'Historical analysis of Jabotinsky''s doctrinal formation, comparison with pre-1923 Zionist territorial conceptions, archaeological/historical scholarship on ancient Israelite borders vs. revisionist map. The claim''s emergence coincides with British Transjordan separation (1922) — suggesting reactive construction.',
    'If constructed, the constraint is a false summit candidate (mountain claim with beneficiaries) triggering FSM. If natural, the high extraction/suppression metrics represent the cost of realizing a genuine right, not extractive overhead. The engine''s FSM signature would reclassify based on beneficiary presence + metric profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_right_vs_constructed_claim, conceptual, 'Natural-law vs constructed ambiguity in the territorial claim''s justification').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the Palestinian Arab population''s subjection to the Iron Wall constraint maintained primarily by structural military-legal suppression (checkpoints, permits, settlements, military law) or by internalized suppression (resignation, collaboration, ideological defeat, ''security coordination'')?',
    'Post-exit suppression trajectory: if suppression persists after direct military control relaxes (e.g., Oslo Areas A/B, Gaza disengagement), reclassify as partially internalized. Compare Palestinian Authority security coordination with pre-Oslo direct rule. Measure resistance continuity across governance modes.',
    'If internalized, effective suppression is higher than structural measure suggests — the target carries the constraint''s logic internally. This would increase χ for Palestinian payer seats beyond the engine''s structural derivation. The omega routes this ambiguity to the engine''s attention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs internalized suppression mechanism in Palestinian subjection').

omega_variable(
    iron_wall_doctrine_empirical_validity,
    'Does the Iron Wall doctrine''s core empirical claim — that Arab acceptance can ONLY be compelled by overwhelming force and NEVER achieved voluntarily — hold under historical test? Egypt (1979), Jordan (1994), PLO (1988/1993), Arab Peace Initiative (2002) suggest voluntary acceptance IS possible under different terms.',
    'Compare revisionist prediction (no voluntary agreement ever) with historical outcomes: peace treaties, recognition, security cooperation. Assess whether these constitute ''acceptance'' on revisionist terms or falsify the doctrine. The doctrine''s predictive failure would undermine its coordination framing.',
    'If falsified, the constraint''s claimed coordination function collapses — it becomes pure extraction (snare) maintained by ideological commitment to a failed strategic premise. The theater_ratio would rise as enforcement continues without strategic justification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(iron_wall_doctrine_empirical_validity, empirical, 'Empirical validity of the Iron Wall doctrine''s core strategic claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__revisionist_zionism_reading, 1923, 1967).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1923, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1923, 0.1).
narrative_ontology:measurement(jewi_tr_t1930, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1930, 0.15).
narrative_ontology:measurement(jewi_tr_t1937, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1937, 0.2).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1948, 0.22).
narrative_ontology:measurement(jewi_tr_t1956, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1956, 0.25).
narrative_ontology:measurement(jewi_tr_t1967, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1967, 0.28).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1923, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1923, 0.35).
narrative_ontology:measurement(jewi_be_t1930, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1930, 0.42).
narrative_ontology:measurement(jewi_be_t1937, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1937, 0.58).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1948, 0.75).
narrative_ontology:measurement(jewi_be_t1956, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1956, 0.78).
narrative_ontology:measurement(jewi_be_t1967, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1967, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1923, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1923, 0.55).
narrative_ontology:measurement(jewi_su_t1930, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1930, 0.65).
narrative_ontology:measurement(jewi_su_t1937, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1937, 0.78).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1948, 0.88).
narrative_ontology:measurement(jewi_su_t1956, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1956, 0.9).
narrative_ontology:measurement(jewi_su_t1967, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1967, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__revisionist_zionism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__revisionist_zionism_reading, 0.12).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, palestinian_displacement_constraint).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, settler_colonial_land_expropriation).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, israeli_military_occupation_regime).

% DUAL FORMULATION NOTE:
% This constraint is the revisionist_zionism_reading of the jewish_territorial_claim kernel. It decomposes the kernel's maximalist, militarized, non-consensual variant. Sibling readings instantiate different constraints with lower ε (political: partition acceptance; labor: socialist coordination; cultural: non-sovereign spiritual center). The revisionist reading's high extraction/suppression is reading-specific, not kernel-intrinsic. Network edges reflect ideological descent (from political_zionism), operational coupling (with labor_zionism's settlement apparatus), and downstream effects (occupation regime, displacement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_territorial_claim__revisionist_zionism_reading, institutional, 0.15).
constraint_indexing:directionality_override(jewish_territorial_claim__revisionist_zionism_reading, powerless, 0.95).
constraint_indexing:directionality_override(jewish_territorial_claim__revisionist_zionism_reading, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
