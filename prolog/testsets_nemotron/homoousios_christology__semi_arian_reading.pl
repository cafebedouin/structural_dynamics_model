% ============================================================================
% CONSTRAINT STORY: homoousios_christology__semi_arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__semi_arian_reading, []).

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
 *   constraint_id: homoousios_christology__semi_arian_reading
 *   human_readable: Semi-Arian Homoiousios Compromise (Christ of Similar Substance)
 *   domain: historical_theology/ecclesiastical_politics
 *
 * SUMMARY:
 *   The Semi-Arian (homoiousios) reading emerged at the Council of Sirmium
 *   (351, 357, 358) as a terminological compromise between the Pro-Nicene
 *   homoousios (consubstantial) and the Arian heteroousios/anomoios
 *   (dissimilar/unlike substance). It proposed Christ is 'of similar
 *   substance' (homoiousios) to the Father — preserving distinction while
 *   affirming likeness. The formula functioned as an imperial mediation tool
 *   under Constantius II, enforced through synodal subscription and episcopal
 *   exile. After Julian's accession (361) and especially under Theodosius I,
 *   the compromise dissolved: Constantinople I (381) canonized homoousios and
 *   condemned homoiousios as heretical. The constraint operated as a scaffold
 *   — a temporary coordination structure with a declared sunset (imperial
 *   unity) that was absorbed into the Pro-Nicene settlement.
 *
 * KEY AGENTS:
 *   - imperial_mediators: agenda_setter (institutional/generational/arbitrage/global) — Constantius II, Valens; enforced homoiousios subscription to maintain church unity as imperial cohesion
 *   - moderate_bishops: beneficiary/payer (organized/biographical/constrained/regional) — Basil of Ancyra, George of Laodicea; gained episcopal legitimacy under imperial protection but bore theological incoherence costs
 *   - homoians: beneficiary (organized/biographical/mobile/continental) — Acacius of Caesarea, Eudoxius; occupied the homoiousios center as a stable position until Homoian shift to anomoios
 *   - pro_nicene_partisans: payer/victim (powerful/biographical/trapped/national) — Athanasius, Hilary of Poitiers, Basil of Caesarea; exiled, deposed, condemned for refusing homoiousios subscription
 *   - arian_partisans: payer/victim (organized/biographical/constrained/regional) — Aetius, Eunomius; marginalized by homoiousios as insufficiently subordinationist, later condemned as anomoeans
 *   - theological_observers: observer (analytical/civilizational/analytical/universal) — later historians, dogmatic theologians; analyze the formula's structural function without stake in its enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__semi_arian_reading, 0.38).
domain_priors:suppression_score(homoousios_christology__semi_arian_reading, 0.42).
domain_priors:theater_ratio(homoousios_christology__semi_arian_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__semi_arian_reading, scaffold).
narrative_ontology:human_readable(homoousios_christology__semi_arian_reading, "Semi-Arian Homoiousios Compromise (Christ of Similar Substance)").
narrative_ontology:topic_domain(homoousios_christology__semi_arian_reading, "historical_theology/ecclesiastical_politics").

domain_priors:requires_active_enforcement(homoousios_christology__semi_arian_reading).
narrative_ontology:has_sunset_clause(homoousios_christology__semi_arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__semi_arian_reading, '7aa8f029-0cdc-4b68-8291-39fb84627514').
narrative_ontology:cs_kernel_codification('7aa8f029-0cdc-4b68-8291-39fb84627514', formalized).
narrative_ontology:cs_authority_grounding('7aa8f029-0cdc-4b68-8291-39fb84627514', lineage).
narrative_ontology:cs_interpretation_layer_present('7aa8f029-0cdc-4b68-8291-39fb84627514').
narrative_ontology:cs_reading_relation('7aa8f029-0cdc-4b68-8291-39fb84627514', homoousios_christology__pro_nicene_reading, influences).
narrative_ontology:cs_reading_relation('7aa8f029-0cdc-4b68-8291-39fb84627514', homoousios_christology__arian_reading, influences).
narrative_ontology:cs_axiom('7aa8f029-0cdc-4b68-8291-39fb84627514', foundational, similar_substance_sufficient_for_unity).
narrative_ontology:cs_axiom_status(similar_substance_sufficient_for_unity, overridden).
narrative_ontology:cs_axiom_grounding('7aa8f029-0cdc-4b68-8291-39fb84627514', similar_substance_sufficient_for_unity, conventional).
narrative_ontology:cs_axiom('7aa8f029-0cdc-4b68-8291-39fb84627514', foundational, imperial_authority_mediates_doctrinal_settlement).
narrative_ontology:cs_axiom_status(imperial_authority_mediates_doctrinal_settlement, overridden).
narrative_ontology:cs_axiom_grounding('7aa8f029-0cdc-4b68-8291-39fb84627514', imperial_authority_mediates_doctrinal_settlement, conventional).
narrative_ontology:cs_reference_frame('7aa8f029-0cdc-4b68-8291-39fb84627514', nicene_creed_325_as_ambiguous_foundation).
narrative_ontology:cs_drift_state('7aa8f029-0cdc-4b68-8291-39fb84627514', constantinian_settlement_355_360, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7aa8f029-0cdc-4b68-8291-39fb84627514', '').
narrative_ontology:cs_kernel_id(homoousios_christology__semi_arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, imperial_mediators).
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, moderate_bishops).
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, homoians).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, pro_nicene_partisans).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, arian_partisans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, moderate_bishops).
narrative_ontology:constraint_vindicates(homoousios_christology__semi_arian_reading, unity_without_identity_doctrine).
narrative_ontology:constraint_vindicates(homoousios_christology__semi_arian_reading, imperial_church_mediation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Roman emperors (Constantius II, Valens) who convened councils, enforced subscription to homoiousios formulas, and exiled dissenting bishops. They set the theological agenda to secure imperial unity, collecting political stability rents from church cohesion. Can switch formulas (Julian's tolerance, Theodosius' Nicene turn) — arbitrage exit.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, imperial_mediators, agenda_setter,
    institutional, generational, arbitrage, global).

% Bishops like Basil of Ancyra and George of Laodicea who accepted homoiousios under imperial pressure. Gained legitimate episcopal sees and imperial patronage (beneficiary) but bore the cost of theological incoherence — defending a formula they knew was unstable against both Nicene and Arian critique (payer). Exit options constrained: schism meant loss of see; submission meant doctrinal compromise.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, moderate_bishops, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__semi_arian_reading, moderate_bishops, payer).

% The 'Homoian' faction (Acacius of Caesarea, Eudoxius of Antioch) who occupied the homoiousios center as a stable ecclesiastical position. Benefited from imperial recognition as the 'moderate' majority. Exit was mobile: they could shift toward anomoios (Aetius/Eunomius) when imperial winds changed, as many did post-360.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, homoians, beneficiary,
    organized, biographical, mobile, continental).

% Athanasius of Alexandria, Hilary of Poitiers, Basil of Caesarea, Gregory of Nazianzus — bishops and theologians who refused homoiousios as a betrayal of Nicaea. Subjected to repeated exile, deposition, and anathema under Constantius and Valens. Exit was trapped: their identity was fused with the homoousios confession; subscription would dissolve their epistemic and ecclesial self. Identity-locked by conviction, not merely constrained by circumstance.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, pro_nicene_partisans, payer,
    powerful, biographical, trapped, national).

% Aetius, Eunomius, and the 'Anomoean' wing who rejected homoiousios as insufficiently subordinationist — the Son is unlike (anomoios) the Father in substance. Marginalized by the compromise as 'extremists,' later condemned at Constantinople. Exit constrained: could radicalize further (anomoios) or submit to homoiousios; neither was a full exit from extraction.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, arian_partisans, payer,
    organized, biographical, constrained, regional).

% Later historians (Socrates, Sozomen, Theodoret), dogmatic theologians, and modern scholars who analyze the formula's structural function without stake in its enforcement. Analytical exit: they can adopt any interpretive frame; costs/benefits of analysis are symmetric.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, theological_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a terminological middle ground (homoiousios) that allowed imperial authorities to claim church unity without requiring either full Nicene submission or explicit Arian subordinationism — solving the coordination problem of imperial-church cohesion under Constantius II.
% TRANSFER_FUNCTION: Moves episcopal legitimacy and imperial patronage toward bishops who subscribe to homoiousios; moves exile, deposition, and anathema toward bishops who refuse (Pro-Nicene and Anomoean). The transfer is ecclesiastical office and imperial protection for terminological compliance.
% ABSENT_VOICES: Laity and monastic communities who had no voice in conciliar subscriptions but bore the pastoral consequences of episcopal turnover; Western bishops (especially after 360) who were structurally excluded from Eastern conciliar machinery but later became decisive at Constantinople.
% DISAPPEARANCE_RATIONALE: If homoiousios enforcement vanished in 358, the Pro-Nicene and Arian poles would have collided directly without the compromise buffer — likely accelerating the schism violence that Constantius sought to manage. The 381 sunset rearranged the world by absorbing the compromise into Nicene orthodoxy; a premature disappearance would have produced a different, bloodier rearrangement.
% FOUNDING_PROBLEM: The post-Nicene church faced persistent schism between homoousios and heteroousios parties, threatening imperial unity. The homoiousios formula was built to solve the coordination problem: how to maintain a single imperial church without forcing either party to abandon its core conviction.
% FOUNDING_PROBLEM_CORROBORATION: Imperial mediators (Constantius II's letters) attest the problem was live and the formula solved it. Pro-Nicene partisans (Athanasius, Hilary) attest the problem was pseudosolved — the formula papered over an unresolvable doctrinal contradiction. Arian partisans (Eunomius) attest the formula was a Nicene trap. No neutral corroboration exists; all witnesses are partisan. The status remains contested because the 'problem' (church unity) was eventually solved by Pro-Nicene victory, not by the compromise.
narrative_ontology:disappearance_verdict(homoousios_christology__semi_arian_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__semi_arian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__semi_arian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(homoousios_christology__semi_arian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__semi_arian_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__semi_arian_reading_tests).
:- end_tests(homoousios_christology__semi_arian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) is moderate: the formula demanded terminological subscription under penalty of exile/deposition but offered a genuine middle ground that reduced violence compared to polar enforcement. Suppression (0.42) reflects active enforcement machinery (imperial synods, episcopal exile) but lower than Pro-Nicene enforcement (which deployed full conciliar anathema and state coercion). Theater ratio (0.28) captures performative unity — the formula was publicly celebrated as peace while privately recognized as unstable by all parties. Accessibility collapse (0.35) is partial: alternatives (homoousios, anomoios) remained intellectually available and organizationally active throughout. Resistance (0.55) is high: both Pro-Nicene and Arian factions resisted the compromise as theologically incoherent, generating sustained polemical literature and schismatic networks. The claimed_type scaffold reflects the explicit temporality: the formula was advanced as a provisional unity measure pending fuller resolution, with its 'sunset' at a general council (realized at Constantinople 381).
 *
 * PERSPECTIVAL GAP:
 *   From the imperial mediator's seat, homoiousios is coordination (rope-like) — it solves the schism problem at acceptable enforcement cost. From Pro-Nicene and Arian seats, it is extraction (snare-like) — it forces terminological betrayal of core conviction. The engine computes this divergence from structural data: imperial power + arbitrage exit = low directionality (beneficiary); partisan power + trapped exit = high directionality (target). The moderate bishops sit in genuine dual position: beneficiaries of imperial recognition, payers of theological coherence.
 *
 * DIRECTIONALITY LOGIC:
 *   Imperial mediators are structural beneficiaries (d ~0.15): they set the agenda, collect unity rents, and hold arbitrage exit (can switch formulas). Moderate bishops are dual (d ~0.45): gain episcopal security but lose theological autonomy; exit is constrained (schism or submission). Homoians are beneficiaries (d ~0.25): their position becomes the imperial standard; exit is mobile (can shift to anomoios). Pro-Nicene partisans are targets (d ~0.85): forced to choose exile or subscription; exit is trapped (identity-locked by Nicene conviction). Arian partisans are targets (d ~0.75): squeezed between homoiousios and anomoios; exit is constrained (can radicalize to anomoios or submit). Theological observers are analytical (d=0.5): symmetric costs/benefits of analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (imperial church unity via terminological compromise) outlived its function when the Pro-Nicene faction demonstrated that unity could be achieved on homoousios terms without the compromise. The scaffold's sunset clause (general council ratification) was triggered at Constantinople 381, but the absorption was coercive — Pro-Nicene victory reframed the compromise as heresy rather than fulfilling it. This creates mandatrophy ambiguity: the constraint's declared function (unity) was achieved, but by a different formula that retroactively criminalized the scaffold. The omega on sunset_clause_effectiveness captures this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_naturalness_ambiguity,
    'Is the homoiousios formula a genuine theological coordination mechanism or a constructed compromise masking continued Arian subordinationism?',
    'Analysis of the formula''s reception across episcopal networks 355-381 and its doctrinal stability under pressure from both Pro-Nicene and Arian polemicists; trace whether it functions as a stable intermediate position or a transitional mask.',
    'If genuine coordination, the scaffold classification holds with its sunset clause (Council of Constantinople 381); if constructed mask, it reclassifies as tangled_rope or snare with higher extraction from enforced terminological ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_naturalness_ambiguity, conceptual, 'Whether the compromise position is structurally stable or a temporary rhetorical shelter').

omega_variable(
    enforcement_asymmetry_measurement,
    'Did imperial enforcement of homoiousios extract more heavily from Pro-Nicene or Arian parties, and does this extraction pattern distinguish the Semi-Arian reading from its siblings?',
    'Comparative study of exile patterns, conciliar depositions, and property confiscations under Constantius II and Valens vs. Julian and Theodosius I; measure enforcement intensity per faction.',
    'Asymmetric extraction favoring one pole would confirm tangled_rope structure; symmetric low enforcement supports scaffold; concentration on one faction supports snare dynamics for that faction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_asymmetry_measurement, empirical, 'Distribution of enforcement burden across the three christological parties').

omega_variable(
    sunset_clause_effectiveness,
    'Was the ''sunset'' at Constantinople 381 a genuine structural transition or a Pro-Nicene capture that reframed the compromise as heresy?',
    'Trace the post-381 fate of homoiousios theologians (Eunomius, Basil of Ancyra successors); determine whether absorption was voluntary doctrinal convergence or coercive reclassification.',
    'Genuine sunset validates scaffold with mandatrophy_resolved; coercive capture means the constraint persisted as snare/tangled_rope under new labeling, with extraction continuing in transformed form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_effectiveness, empirical, 'Whether the 381 terminus represents authentic resolution or rebranding of extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__semi_arian_reading, 355, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t355, homoousios_christology__semi_arian_reading, theater_ratio, 355, 0.15).
narrative_ontology:measurement(homo_tr_t359, homoousios_christology__semi_arian_reading, theater_ratio, 359, 0.22).
narrative_ontology:measurement(homo_tr_t360, homoousios_christology__semi_arian_reading, theater_ratio, 360, 0.31).
narrative_ontology:measurement(homo_tr_t363, homoousios_christology__semi_arian_reading, theater_ratio, 363, 0.28).
narrative_ontology:measurement(homo_tr_t370, homoousios_christology__semi_arian_reading, theater_ratio, 370, 0.25).
narrative_ontology:measurement(homo_tr_t378, homoousios_christology__semi_arian_reading, theater_ratio, 378, 0.27).
narrative_ontology:measurement(homo_tr_t381, homoousios_christology__semi_arian_reading, theater_ratio, 381, 0.28).

% Extraction over time
narrative_ontology:measurement(homo_be_t355, homoousios_christology__semi_arian_reading, base_extractiveness, 355, 0.28).
narrative_ontology:measurement(homo_be_t359, homoousios_christology__semi_arian_reading, base_extractiveness, 359, 0.35).
narrative_ontology:measurement(homo_be_t360, homoousios_christology__semi_arian_reading, base_extractiveness, 360, 0.42).
narrative_ontology:measurement(homo_be_t363, homoousios_christology__semi_arian_reading, base_extractiveness, 363, 0.38).
narrative_ontology:measurement(homo_be_t370, homoousios_christology__semi_arian_reading, base_extractiveness, 370, 0.33).
narrative_ontology:measurement(homo_be_t378, homoousios_christology__semi_arian_reading, base_extractiveness, 378, 0.36).
narrative_ontology:measurement(homo_be_t381, homoousios_christology__semi_arian_reading, base_extractiveness, 381, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t355, homoousios_christology__semi_arian_reading, suppression_requirement, 355, 0.35).
narrative_ontology:measurement(homo_su_t359, homoousios_christology__semi_arian_reading, suppression_requirement, 359, 0.48).
narrative_ontology:measurement(homo_su_t360, homoousios_christology__semi_arian_reading, suppression_requirement, 360, 0.52).
narrative_ontology:measurement(homo_su_t363, homoousios_christology__semi_arian_reading, suppression_requirement, 363, 0.45).
narrative_ontology:measurement(homo_su_t370, homoousios_christology__semi_arian_reading, suppression_requirement, 370, 0.38).
narrative_ontology:measurement(homo_su_t378, homoousios_christology__semi_arian_reading, suppression_requirement, 378, 0.42).
narrative_ontology:measurement(homo_su_t381, homoousios_christology__semi_arian_reading, suppression_requirement, 381, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__semi_arian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_christology__semi_arian_reading, 0.1).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, homoousios_christology__pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, homoousios_christology__arian_reading).

% DUAL FORMULATION NOTE:
% The homoousios_christology kernel decomposes into three constraint stories: arian_reading (Christ created/subordinate, high extraction from Nicene party), semi_arian_reading (homoiousios compromise, scaffold with sunset), pro_nicene_reading (homoousios, identity coordination with post-381 enforcement dominance). Each has distinct ε, beneficiary/victim structure, and temporal profile. The semi_arian_reading structurally influences both siblings: it delays pro_nicene enforcement consolidation (influences) and marginalizes arian radicalization (influences), but forecloses neither within the kernel's commitment framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(homoousios_christology__semi_arian_reading, institutional, 0.15).
constraint_indexing:directionality_override(homoousios_christology__semi_arian_reading, powerful, 0.85).
constraint_indexing:directionality_override(homoousios_christology__semi_arian_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
