% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__covenant_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__covenant_continuity_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__covenant_continuity_reading
 *   human_readable: Covenant-Continuity Reading of Territorial Sovereignty Legitimacy
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested territorial
 *   sovereignty legitimacy kernel: the covenant-continuity reading, which
 *   grounds Jewish sovereignty in the combination of ancient divine covenant,
 *   continuous historical presence (even where demographically thin or absent
 *   for long stretches), and modern international ratification acts (Balfour
 *   Declaration 1917, UN Partition Plan 1947, 1948 establishment). Under this
 *   reading, these modern instruments are understood as RATIFYING a
 *   pre-existing right rather than CREATING a new one, and settlement in the
 *   West Bank is framed as return to ancestral patrimony rather than
 *   colonization of foreign territory. This reading is authored as a clean,
 *   ε-invariant constraint per Rule 1: it does not average over or hedge
 *   against the sibling self-determination reading (which grounds legitimacy
 *   in 19th-20th century Arab demographic majority) or the existential-matrix
 *   reading (which treats the conflict as fundamentally non-juridical and
 *   zero-sum). Those are separate constraint files with their own ε and
 *   stakeholder structures, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - israeli_state_and_citizenry: primary institutional beneficiary and agenda_setter — administers land policy under the covenant-continuity legitimacy frame
 *   - religious_zionist_settlement_movement: identity-locked beneficiary — settlement activity is constituted by the covenant claim itself
 *   - palestinian_arab_residents_displaced_1948 and west_bank_palestinian_landholders: primary targets — bear land loss and residency restriction justified as restoration rather than dispossession
 *   - international_recognition_bodies: historical agenda_setter whose 1917-1948 acts are cited as ratification, now largely inactive as interpreters
 *   - rival_kernel_readings_self_determination_and_existential: excluded — structurally present competing claims not accommodated within this reading's own terms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.58).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.62).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__covenant_continuity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__covenant_continuity_reading, "Covenant-Continuity Reading of Territorial Sovereignty Legitimacy").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__covenant_continuity_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__covenant_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__covenant_continuity_reading, '55861cb0-f20b-402e-b33a-6e3d9cff4813').
narrative_ontology:cs_kernel_codification('55861cb0-f20b-402e-b33a-6e3d9cff4813', distributed).
narrative_ontology:cs_authority_grounding('55861cb0-f20b-402e-b33a-6e3d9cff4813', lineage).
narrative_ontology:cs_interpretation_layer_present('55861cb0-f20b-402e-b33a-6e3d9cff4813').
narrative_ontology:cs_reading_relation('55861cb0-f20b-402e-b33a-6e3d9cff4813', territorial_sovereignty_legitimacy__self_determination_reading, coexists_with).
narrative_ontology:cs_reading_relation('55861cb0-f20b-402e-b33a-6e3d9cff4813', territorial_sovereignty_legitimacy__existential_matrix_reading, influences).
narrative_ontology:cs_axiom('55861cb0-f20b-402e-b33a-6e3d9cff4813', foundational, covenantal_title_survives_demographic_absence).
narrative_ontology:cs_axiom_status(covenantal_title_survives_demographic_absence, holdable).
narrative_ontology:cs_axiom_grounding('55861cb0-f20b-402e-b33a-6e3d9cff4813', covenantal_title_survives_demographic_absence, theological).
narrative_ontology:cs_axiom('55861cb0-f20b-402e-b33a-6e3d9cff4813', foundational, modern_instruments_ratify_antecedent_right).
narrative_ontology:cs_axiom_status(modern_instruments_ratify_antecedent_right, holdable).
narrative_ontology:cs_axiom_grounding('55861cb0-f20b-402e-b33a-6e3d9cff4813', modern_instruments_ratify_antecedent_right, conventional).
narrative_ontology:cs_reference_frame('55861cb0-f20b-402e-b33a-6e3d9cff4813', ancient_covenantal_sovereignty).
narrative_ontology:cs_drift_state('55861cb0-f20b-402e-b33a-6e3d9cff4813', post_1993_oslo_and_post_2000s_international_law_consensus, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('55861cb0-f20b-402e-b33a-6e3d9cff4813', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_and_citizenry).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, religious_zionist_settlement_movement).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, diaspora_jewish_communities_asserting_return_claim).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_arab_residents_displaced_1948).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, west_bank_palestinian_landholders).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_refugee_descendants).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, continuous_jewish_presence_claim).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, balfour_and_partition_as_ratification_not_creation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds sovereign control over the contested territory, administers land and settlement policy, and invokes covenant plus continuous presence plus international instruments (Balfour, Partition, 1948 statehood) as the legitimating chain. Controls the legal and military apparatus that gives the covenant-continuity reading practical effect on the ground.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_and_citizenry, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_and_citizenry, agenda_setter).

% Builds and defends settlements in the West Bank framed explicitly as return to ancestral land rather than new acquisition. Their entire political and religious identity is fused to the covenant claim; abandoning the reading would dissolve the movement's reason for existing, not merely change its policy preference.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, religious_zionist_settlement_movement, beneficiary,
    organized, civilizational, identity_locked, regional).

% Draw identity, immigration rights (right of return / aliyah), and diplomatic support from the covenant-continuity narrative without directly administering land. Benefit from the legitimacy claim's international resonance but bear none of the day-to-day enforcement costs.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, diaspora_jewish_communities_asserting_return_claim, beneficiary,
    organized, generational, mobile, global).

% Lost land, homes, and residency rights in the events surrounding 1948, which the covenant-continuity reading frames as the restoration of pre-existing sovereignty rather than a dispossession requiring remedy. Their claims to the same land are treated as subordinate to or extinguished by the covenant-continuity legitimacy chain.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_arab_residents_displaced_1948, payer,
    powerless, biographical, trapped, regional).

% Face ongoing land expropriation, permit denial, and settlement expansion justified under the return-not-colonization frame. Legal recourse is structured by the very authority whose legitimacy rests on the reading that disadvantages their claims; exit from the territory or the legal system is not realistically available.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, west_bank_palestinian_landholders, payer,
    powerless, biographical, trapped, local).

% Live in diaspora or refugee status with claims to return that are structurally incompatible with the covenant-continuity account, since that account treats 1948 as ratification of a pre-existing sovereign right rather than the creation of a contestable new state whose founding displaced them.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_refugee_descendants, payer,
    powerless, generational, trapped, regional).

% The League of Nations Mandate framework, the Balfour Declaration's drafters, and the UN Partition Plan's authors supplied the modern instruments the covenant-continuity reading cites as ratifying an antecedent right. These bodies acted as adjudicating authority at critical junctures but no longer function as active interpreters of the underlying covenant claim itself.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_recognition_bodies, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_recognition_bodies, observer).

% Advocates and populations holding the self-determination reading (grounded in 19th-20th century demographic majority) or the existential-matrix reading (grounded in survival necessity rather than juridical history) are structurally present in the same territory and discourse but are not accommodated within the covenant-continuity legitimacy chain, which treats their claims as later-arising or non-juridical.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, rival_kernel_readings_self_determination_and_existential, excluded,
    organized, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, transmissible legitimacy narrative that lets a dispersed people re-establish sovereign continuity across a multi-millennial gap, coordinating diaspora return, international diplomatic recognition, and domestic legal administration around a single coherent claim rather than treating 1948 as an unprecedented, contestable founding.
% TRANSFER_FUNCTION: Moves land, residency rights, and political authority from the Arab population resident in the territory during the mandate and pre-1948 period to the Jewish state and its citizens and settlers, using the covenant-continuity narrative to characterize this transfer as restoration rather than displacement.
% ABSENT_VOICES: Palestinian claimants whose historical presence and demographic majority during the modern period ground the self-determination reading are structurally excluded from the covenant-continuity legitimacy chain's own terms — their claim is treated as arising too late in the reading's temporal frame to compete with the ancient covenant. Advocates of the existential-matrix reading, who would reject the juridical framing entirely, are also absent from this reading's discourse.
% DISAPPEARANCE_RATIONALE: If the covenant-continuity legitimacy claim were withdrawn overnight, Israeli statehood would not cease to exist (it rests on additional grounds — effective control, international recognition, self-determination arguments), but the specific settlement movement and diaspora-return apparatus that depend on the covenant framing for their moral and political justification would lose their primary legitimating narrative, forcing a rearrangement of settlement policy and immigration law around alternative justifications. Israeli state defenders dispute this claim; Palestinian advocates and international law scholars argue the practical territorial arrangement would remain largely unchanged in the short term but lose a key rhetorical shield.
% FOUNDING_PROBLEM: The problem of establishing a legally and morally defensible basis for renewed Jewish sovereignty after nearly two millennia of dispersion, in a territory continuously inhabited by others, requiring a legitimacy claim that could bridge the demographic gap and connect ancient presence to modern statehood.
% FOUNDING_PROBLEM_CORROBORATION: Israeli state historiography and religious-Zionist scholarship attest the founding problem remains live (ongoing need to legitimate sovereignty against continuing challenge). Independent international law scholars and UN human rights bodies — outside the beneficiary set — attest that the founding problem as originally framed (establishing juridical continuity) has been substantially superseded by the modern legal reality of belligerent occupation and unresolved self-determination claims, and that the covenant-continuity framing now functions primarily to resist land restitution and refugee return rather than to solve an open juridical question.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__covenant_continuity_reading, contested).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__covenant_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a substantial but not extreme level (0.58 by 2024) because the covenant-continuity reading, on its own terms, is not purely predatory — it does genuine coordination work (stabilizing diaspora return, providing a transmissible legitimacy narrative across a multi-millennial discontinuity) while also functioning, in its application to land and settlement policy, to transfer real assets away from a resident population whose competing claim it structurally discounts. Suppression is authored higher (0.62) and rising sharply after 1948 and 1967 because maintaining the reading against a resident population with a rival claim requires active legal, administrative, and at times military enforcement — this is not a passive narrative, it is one backed by state power. Theater ratio is modest and slowly rising (0.28 by 2024), reflecting that the covenant-continuity narrative is not primarily performative — it does substantive legitimating work — but an increasing share of its invocation (in diplomatic and legal contexts) functions rhetorically rather than as the operative basis for actual policy decisions, which increasingly rest on security and demographic arguments instead. Accessibility collapse is moderate (0.45): unlike a mountain, alternative legitimacy framings (self-determination, existential-matrix, negotiated partition) remain visibly available and actively argued, so alternatives have not collapsed the way they would under a genuine natural-law claim. Resistance is high (0.78) because the reading is continuously and vigorously contested by international legal scholars, Palestinian advocates, and portions of the international community.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (Israeli state, settlement movement, diaspora), the reading computes as a tangled_rope at most — largely coordination (restoring severed continuity, providing stable legitimacy) with some enforcement cost. From the payer seats (displaced residents, landholders, refugee descendants), the same structure computes closer to snare: enforced land transfer legitimated by a claim they have no standing to contest on its own terms. This divergence is exactly the seat-divergence the engine is built to detect — it is not resolved by picking one seat's view as correct; both are structurally accurate readings of the SAME constraint from different positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state and its citizenry sit near the beneficiary end: they hold and exercise the sovereignty the reading legitimates, with strong exit/mobility (arbitrage) via diplomatic and military capacity. The religious settlement movement is also a beneficiary but is identity-locked rather than mobile — their exit option is constrained not by external barriers but by the fact that abandoning the covenant claim would dissolve their reason for existing. Diaspora communities benefit from the narrative's international resonance without bearing enforcement costs, giving them mobile exit. Displaced Palestinian residents and West Bank landholders are the clearest targets: trapped exit options, powerless power atom, and the reading directly discounts their competing claim as later-arising or as pre-empted by covenant continuity. International recognition bodies acted as agenda-setters at critical historical junctures (1917, 1947) but now sit more as inactive historical authors of instruments the reading cites — they are not actively re-interpreting the covenant claim today.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — establishing a legitimacy bridge across a two-thousand-year demographic gap — was, on its own terms, a genuine juridical and political problem in 1917-1948. Whether that problem remains live (as Israeli and religious-Zionist sources maintain) or has been substantially superseded by settled statehood and now functions primarily to resist land restitution (as independent international law scholarship argues) is exactly the founding_problem_status contest authored above. The mismatch between founding_problem_status=contested and disappearance_verdict=contested is itself diagnostic: this is not a case where an obviously dead mandate persists by inertia (piton), nor a case where withdrawal would leave the world entirely unchanged (mountain-like). It is a live, actively defended, actively contested legitimacy claim doing real coordination work for its beneficiaries while imposing real, enforced costs on its victims — the tangled_rope classification is chosen because both the coordination function and the asymmetric extraction are structurally present, not because one is decoration for the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ratification_vs_creation_of_right,
    'Do the Balfour Declaration, UN Partition Plan, and 1948 establishment RATIFY a pre-existing covenantal sovereign right, or CREATE a new sovereign right where none existed in international law prior to these instruments?',
    'This is not resolvable by further historical evidence alone — it depends on whether one accepts a theological/covenantal claim as a jurisprudentially cognizable source of title, which is a framing question rather than an empirical one. Comparative analysis of how international law treats other ancient-continuity claims to sovereignty (e.g., contested claims elsewhere) could narrow but not close the question.',
    'If the modern instruments are ratification, the covenant-continuity reading''s extractiveness score should be read as largely coordination cost (restoring a right, not creating one); if they are creation, the same instruments constitute the founding act of a new transfer, and the extraction measured here is closer to the reading''s true structural weight. This is the central conceptual fork the reading and its self-determination sibling divide on.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ratification_vs_creation_of_right, conceptual, 'Whether 1917-1948 instruments ratified an antecedent right or created a new one — the reading''s central move.').

omega_variable(
    continuous_presence_empirical_status,
    'How continuous, in demographic and political-control terms, was Jewish presence in the territory across the roughly 1,800 years between the Bar Kokhba revolt''s suppression and the onset of modern Zionist immigration?',
    'Historical and archaeological demographic reconstruction across the relevant centuries, cross-checked against tax, census, and pilgrimage records where they exist.',
    'If continuous presence is empirically thin (small, fluctuating minority populations across most of the interval), the ''continuity'' component of the reading''s legitimacy chain is weaker than the covenant component alone would suggest, which would push the reading''s structural weight further toward the theological/promissory axiom and away from an empirically-grounded continuity claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuous_presence_empirical_status, empirical, 'Empirical thinness or robustness of the continuous-presence claim underlying the reading.').

omega_variable(
    settlement_return_vs_colonization_framing,
    'Is West Bank settlement construction structurally a ''return'' to ancestral patrimony (as this reading frames it) or a form of colonization of territory under belligerent occupation (as international law bodies and the self-determination reading frame it)?',
    'This is adjudicated differently depending on which body of law and which historical frame is treated as authoritative (international humanitarian law''s occupation framework vs. the covenant-continuity legitimacy chain) — it is not resolvable by additional fact-finding alone, since the same settlement activity is described identically at the level of physical fact and divergently at the level of legitimating frame.',
    'If ''colonization'' framing prevails structurally, the extractiveness and victim-harm measured in this story would be understated relative to how international law characterizes the same activity; if ''return'' framing prevails, the coordination function (restoring severed connection to ancestral land) is understated by outside observers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_return_vs_colonization_framing, conceptual, 'Whether settlement activity is structurally return or colonization — the reading''s most consequential downstream policy implication.').

omega_variable(
    identity_lock_dissolution_scenario,
    'If the covenant-continuity legitimacy frame were successfully displaced (e.g., by binding international adjudication or a negotiated settlement grounded in a different legitimacy theory), would the religious-Zionist settlement movement''s identity-locked commitment dissolve, transform, or intensify?',
    'Comparative study of analogous identity-fused political movements after their founding legitimacy narrative was juridically or politically overturned (e.g., other religiously-grounded territorial movements facing adverse rulings) to establish a base rate for dissolution vs. radicalization.',
    'If identity-lock intensifies resistance rather than dissolving it, the classification''s enforcement/suppression trajectory would be expected to continue rising even under legal reversal, meaning the tangled_rope''s active-enforcement requirement would persist independent of the reading''s formal legitimacy status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_dissolution_scenario, empirical, 'Whether identity-locked beneficiaries would dissolve or intensify commitment if the reading were displaced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__covenant_continuity_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1917, 0.15).
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1948, 0.18).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(terr_tr_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1993, 0.22).
narrative_ontology:measurement(terr_tr_t2005, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(terr_tr_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(terr_be_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1917, 0.32).
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1948, 0.44).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1967, 0.52).
narrative_ontology:measurement(terr_be_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1993, 0.5).
narrative_ontology:measurement(terr_be_t2005, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(terr_be_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1917, 0.25).
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1948, 0.48).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1967, 0.58).
narrative_ontology:measurement(terr_su_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1993, 0.55).
narrative_ontology:measurement(terr_su_t2005, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(terr_su_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__covenant_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__self_determination_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the territorial_sovereignty_legitimacy kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle: covenant_continuity_reading (this file, tangled_rope — genuine coordination via legitimacy-bridging plus asymmetric extraction via land/residency transfer), self_determination_reading (grounds legitimacy in 19th-20th century Arab demographic majority — expected to have a substantially different beneficiary/victim structure), and existential_matrix_reading (treats legitimacy as non-juridical and the conflict as zero-sum regardless of historical argument — expected to resist conventional beneficiary/victim decomposition since it denies the juridical framing that grounds the other two readings). The three readings are linked bidirectionally in intent; this file declares the edges to its two siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
