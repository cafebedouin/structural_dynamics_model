% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__incoherent_bundle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__incoherent_bundle, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: kami_buddha_ontology__incoherent_bundle
 *   human_readable: Shinbutsu-shugo as Institutionally Sustained Incoherent Bundle
 *   domain: religious/philosophical/institutional
 *
 * SUMMARY:
 *   Shinbutsu-shugo (kami-buddha fusion) is conventionally presented as a
 *   coherent theological framework integrating kami and buddhas into a single
 *   ontology. This reading challenges that presentation: the framework is
 *   instead a institutionally sustained incoherence—a bundle of contradictory
 *   commitments (fusion and separation, hierarchical and reciprocal,
 *   systematized and chaotic) held together by institutional power, practical
 *   ritual efficacy, and the sunk cost of centuries of embedded identity. The
 *   constraint extracts from lay practitioners and philosophical
 *   coherence-seekers while benefiting Buddhist institutions, shrine
 *   priesthoods, and state religious authority. Temporal measurements show
 *   rising extractiveness and theater ratio: as institutional pressure for
 *   coherence has lessened (the founding problem is dead), the arrangement's
 *   survival increasingly depends on theatrical maintenance of incoherence
 *   rather than living institutional necessity.
 *
 * KEY AGENTS:
 *   - institutional_buddhism: primary institutional beneficiary (temples, priest lineages) — maintains fusion claim while practicing domain partition
 *   - shrine_priesthood: co-beneficiary and joint administrator — identity fused with maintenance of coexistence
 *   - state_religious_authority: institutional agenda-setter — enforces registration and administrative systems that sustain the bundle
 *   - theological_coherence_seekers: targets of extraction — bear cognitive load of unresolved contradiction, lack power to force clarification
 *   - lay_practitioners: most-constrained payer — trapped in dual practice, cannot exit, internalize suppression
 *   - reformation_scholars: structurally excluded — their alternative frameworks (separation or explicit hierarchy) threatened institutional beneficiaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, 0.68).
domain_priors:suppression_score(kami_buddha_ontology__incoherent_bundle, 0.72).
domain_priors:theater_ratio(kami_buddha_ontology__incoherent_bundle, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, extractiveness, 0.68).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__incoherent_bundle, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__incoherent_bundle, "Shinbutsu-shugo as Institutionally Sustained Incoherent Bundle").
narrative_ontology:topic_domain(kami_buddha_ontology__incoherent_bundle, "religious/philosophical/institutional").

domain_priors:requires_active_enforcement(kami_buddha_ontology__incoherent_bundle).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__incoherent_bundle, '26c298e2-0c14-4202-86ea-ea299bf0c9ee').
narrative_ontology:cs_kernel_codification('26c298e2-0c14-4202-86ea-ea299bf0c9ee', distributed).
narrative_ontology:cs_authority_grounding('26c298e2-0c14-4202-86ea-ea299bf0c9ee', extraction).
narrative_ontology:cs_interpretation_layer_present('26c298e2-0c14-4202-86ea-ea299bf0c9ee').
narrative_ontology:cs_reading_relation('26c298e2-0c14-4202-86ea-ea299bf0c9ee', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('26c298e2-0c14-4202-86ea-ea299bf0c9ee', kami_buddha_ontology__domain_partition, coexists_with).
narrative_ontology:cs_axiom('26c298e2-0c14-4202-86ea-ea299bf0c9ee', foundational, no_single_coherent_ontology).
narrative_ontology:cs_axiom_status(no_single_coherent_ontology, holdable).
narrative_ontology:cs_axiom_grounding('26c298e2-0c14-4202-86ea-ea299bf0c9ee', no_single_coherent_ontology, empirically_contingent).
narrative_ontology:cs_axiom('26c298e2-0c14-4202-86ea-ea299bf0c9ee', foundational, institutional_extraction_through_incoherence).
narrative_ontology:cs_axiom_status(institutional_extraction_through_incoherence, holdable).
narrative_ontology:cs_axiom_grounding('26c298e2-0c14-4202-86ea-ea299bf0c9ee', institutional_extraction_through_incoherence, instrumental).
narrative_ontology:cs_reference_frame('26c298e2-0c14-4202-86ea-ea299bf0c9ee', institutional_coexistence_without_coherence).
narrative_ontology:cs_drift_state('26c298e2-0c14-4202-86ea-ea299bf0c9ee', contemporary_secular_japan, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('26c298e2-0c14-4202-86ea-ea299bf0c9ee', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, institutional_buddhism).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, shrine_priesthood).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, state_religious_authority).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, theological_coherence_seekers).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, lay_practitioners_navigating_contradiction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Buddhist institutions (temples, priest lineages, sects) extract legitimacy and material support from the fusion arrangement. They are primary beneficiaries because shinbutsu-shugo allows them to present themselves as completing rather than competing with Shinto. They administer the arrangement through ritual systematization, theological rationalization attempts, and institutional practice. Exit would require abandoning centuries of embedded institutional identity and the claim that Buddhism completes Japanese spiritual life.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, institutional_buddhism, beneficiary,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, institutional_buddhism, agenda_setter).

% Shinto shrine priests gain institutional legitimacy and material support from the arrangement's claim that kami and buddhas can coexist. They jointly administer the constraint through shrine-temple shared ritual, dual ordinations, and territorial agreements. Their identity is fused with the maintenance of this coexistence. Exit would mean abandoning centuries of priests who held both Shinto and Buddhist credentials simultaneously.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, shrine_priesthood, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, shrine_priesthood, agenda_setter).

% The state (particularly during Edo and Meiji periods) enforces the arrangement through administrative enforcement, shrine-temple registration systems, and religious authority structures. The state benefits from the arrangement's capacity to absorb both Buddhist and Shinto institutions under a single legitimacy framework without requiring coherent theology. Exit is constrained because separation would destabilize both religious institutions and state religious authority simultaneously.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, state_religious_authority, agenda_setter,
    institutional, generational, constrained, national).

% Philosophers, theologians, and educated practitioners who recognize the logical contradiction and seek resolution (separation, systematic synthesis, or explicit hierarchy). They bear the cost of living within and justifying a framework that contradicts itself at fundamental levels. Their exit is constrained by the institutional power of both Buddhism and Shinto, which resist clarification that would disrupt the status quo. They must either suppress awareness of the contradiction or maintain a fragmented intellectual life.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, theological_coherence_seekers, payer,
    moderate, biographical, constrained, national).

% Ordinary Japanese practitioners who perform both Buddhist and Shinto rituals, navigate contradictory instruction from priests about which kami/buddhas govern which domains, and absorb the cognitive load of the unresolved system. They cannot exit because both religions are embedded in family, village, and life-cycle practice. They are trapped in the institutional arrangement's contradictions with minimal power to change the framework. The suppression is partly structural (institutional enforcement) and partly internalized (socialization into accepting contradiction without resolution).
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, lay_practitioners_navigating_contradiction, payer,
    powerless, biographical, trapped, local).

% Religious scholars and reformers from other traditions (especially during early Meiji) who attempted to impose coherence by separating kami and buddhas or establishing systematic hierarchy. They are structurally excluded from the institutional consensus that maintained incoherence. Their alternative frameworks were rejected or suppressed by both Buddhist and Shinto authorities who benefited from the existing contradictions.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, reformation_scholars, excluded,
    moderate, biographical, mobile, national).

% The nonagent entity representing the observed efficacy of shinbutsu-shugo rituals despite theoretical incoherence. Rituals work at the practical level; practitioners experience genuine spiritual benefit; outcomes follow prescribed forms. This practical success is vindicated by the arrangement and sustains its persistence despite lack of theoretical coherence.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, practical_ritual_efficacy, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(kami_buddha_ontology__incoherent_bundle, practical_ritual_efficacy).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__incoherent_bundle, institutional_buddhism).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__incoherent_bundle, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified ritual and institutional framework that allows Japanese religious practice to integrate life-cycle events (birth, marriage, death), seasonal observances, and spiritual authority under a single social structure without requiring a coherent underlying theology. Both Buddhist and Shinto institutions coordinate through shared priests, shared shrines, and shared ritual authority rather than competing for the same spiritual space.
% TRANSFER_FUNCTION: Transfers authority and material support from lay practitioners to both Buddhist temples and Shinto shrines through the requirement that important life events be marked ritually through both traditions. Practitioners must commission services from both institutions, pay both sets of priests, maintain both shrine and temple affiliations. The arrangement routes resources to institutional priesthoods and organizational perpetuation while moving cognitive dissonance and theological frustration to practitioners seeking coherence.
% ABSENT_VOICES: Reformation scholars and philosophical systematizers who attempted to impose coherence are structurally excluded: their alternatives (pure domain partition, explicit honji-suijaku monism, institutional separation) threatened the beneficiaries' institutional positions. Practitioners desiring a coherent theology lack the institutional power to force clarification. No organized voice within either Buddhism or Shinto advocates abandoning the incoherent bundle because both benefit from it.
% DISAPPEARANCE_RATIONALE: If shinbutsu-shugo disappeared and both traditions were forced to separate or cohere, Japanese religious institutional landscape would fracture. Temple and shrine systems built on shared administration would require wholesale reorganization. Life-cycle practices would split between Buddhist and Shinto domains or resolve into explicit hierarchy. Practitioners would face choices about affiliation rather than dual participation. The institutional consensus that has held for centuries would collapse.
% FOUNDING_PROBLEM: Early Japanese religious history involved contact between imported Buddhism and indigenous kami worship. Rather than resolving the ontological relationship (are they the same? different? hierarchical?), institutional actors found that maintaining ambiguity allowed both traditions to coexist and extract support without requiring either to abandon claims or adopt subordinate status. The honji-suijaku framework provided a theoretical cover story (kami as Buddhist manifestations) while practical domain partition (kami for life, buddhas for death) allowed non-overlap. The founding problem was institutional coexistence without coherence.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Japanese religion (outside both Buddhist and Shinto establishments) document that the original institutional pressure for coexistence has largely dissipated: modern Japan is predominantly secular, institutional religions are declining, theological competition is minimal. The founding problem (how to coexist without choosing) no longer exists as a live force. Yet the arrangement persists through institutional inertia, inherited practice, and the sunk cost of centuries of institutional identity built on the contradiction. State-neutral scholarly sources (rather than institutional spokespeople for either tradition) attest the founding crisis is historical, not contemporary.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__incoherent_bundle, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__incoherent_bundle, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__incoherent_bundle, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kami_buddha_ontology__incoherent_bundle, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__incoherent_bundle, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__incoherent_bundle_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__incoherent_bundle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.68 over the interval, reflecting a shift: early institutional coexistence was necessitated by live theological and political stakes; contemporary persistence is increasingly explained by institutional inertia and benefit-to-beneficiaries rather than genuine coordination need. Theater ratio rises from 0.25 to 0.58: the constraint's functional role has atrophied (secular Japan, minimal theological competition) while its ceremonial maintenance has intensified (institutional perpetuation of inherited practice). Suppression is high (0.72) and rising because the arrangement now depends on active enforcement of the incoherence itself—preventing reformation attempts, suppressing intellectual questioning within religious institutions, maintaining institutional compartmentalization. Accessibility_collapse is moderate (0.45) because alternatives exist and are articulated by excluded voices; the collapse is sustained by institutional power, not by the alternatives being genuinely inaccessible. Resistance is moderate-high (0.61) because coherence-seeking remains live within academic scholarship and within questioning practitioners, even though organized resistance is weak.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (institutional Buddhism, shrine priesthood) and the agenda-setter seat (state authority) experience this as successful coordination and institutional legitimacy. They view the incoherence as a feature (flexibility, accommodation) rather than a bug. From the payer seats (lay practitioners, coherence-seekers), the same structure is experienced as enforced intellectual confusion and institutional extraction. The engine should compute this as a substantial divergence in per-seat classification: beneficiaries see rope-like coordination; payers see snare-like extraction masquerading as coherent commitment.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional Buddhism (d ≈ 0.15–0.25): benefits from dual-tradition coexistence, controls significant theology and ritual, has low exit pressure (identity fused with tradition), exercises institutional power over the arrangement. Shrine priesthood (d ≈ 0.2–0.3): similar beneficiary position, moderate exit pressure (alternative would be separation, which is possible but identity-costly), shared power with Buddhism. State authority (d ≈ 0.3–0.4): agenda-setter position, benefits from stability and single-framework legitimacy, moderate exit pressure (could impose separation, but that destabilizes both institutions). Theological coherence-seekers (d ≈ 0.65–0.75): trapped in paying the intellectual cost of unresolved contradiction, zero institutional power, mobile exit (can leave religious institutions or suppress their philosophical concerns). Lay practitioners (d ≈ 0.8–0.9): most-trapped payer, internalized suppression (accept contradiction without question), identity-locked to both traditions through family and community practice, zero individual power. The overrides are minimal because the structural derivation captures the asymmetry: beneficiaries with low exit costs sit near the beneficiary end; trapped payers with zero institutional power sit near the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is 'dead' but the arrangement persists: this is the core mandatrophy signature. The founding problem was institutional coexistence without choosing between traditions—a live problem in medieval and Edo Japan when both institutions were powerful and political stakes were high. In contemporary secular Japan, that problem no longer exists: institutional competition is minimal, theological stakes are low, and institutional pressure for coexistence is weak. Yet shinbutsu-shugo persists through institutional inertia, sunk-cost identity, and the mutual interest of beneficiaries in preserving their inherited authority structures. This is the definition of mandatrophy: a constraint whose mandate (institutional necessity for coexistence, theological pressure to reconcile traditions) has died but whose structure persists through institutional entrenchment. The constraint should be classified as piton (theatrical maintenance of atrophied function) rather than the claimed tangled_rope. However, the claim is tangled_rope because the beneficiary seats genuinely experience ongoing coordination benefits (institutional stability, mutual support, avoided competition), and the payer seats experience ongoing extraction. The engine will compute the mismatch: mandatrophy is present, but the constraint's extractive persistence is real from the payer perspective even if the coordination function has atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incoherence_vs_pragmatic_efficacy,
    'Is shinbutsu-shugo genuinely incoherent at the theoretical level, or does its apparent incoherence mask a coherent practical ontology of functional differentiation that is simply not articulated in formal theological language?',
    'Detailed ethnographic analysis comparing what priests and practitioners claim they believe (theology) versus what they actually do in ritual (practice). If practice is systematically coherent across inconsistent theological statements, the coherence exists at the pragmatic level even if theoretical statements contradict.',
    'If pragmatic coherence is found, the constraint moves from tangled_rope (genuine incoherence extraction) to rope (sophisticated coordination achieved through practical rather than theoretical means). If theoretical and practical levels remain genuinely contradictory, the incoherence reading is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incoherence_vs_pragmatic_efficacy, empirical, 'Whether the constraint''s incoherence is theoretical or practical-level.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) primarily structural (institutional barriers, priest authority, ritual requirements enforced externally) or internalized (practitioners have absorbed the contradiction into their self-concept and do not perceive it as suppression)?',
    'Post-exit analysis: if practitioners who leave Japanese religious institutions report persistent cognitive patterns of contradiction acceptance, suppression is partially internalized. If participants report clarity after exit, suppression is more structural.',
    'If primarily structural: the constraint could be dissolved by removing institutional enforcement. If primarily internalized: the constraint persists even after institutional structures relax, because participants carry the internalized contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Suppression mechanism: institutional enforcement versus internalized acceptance.').

omega_variable(
    institutional_inertia_vs_active_maintenance,
    'Does shinbutsu-shugo persist primarily through institutional inertia (no one is actively maintaining it, it just persists because changing it would be costly) or through active institutional maintenance (beneficiary institutions actively enforce and rationalize the arrangement)?',
    'Historical analysis of institutional reform attempts, regulatory interventions, and deliberate institutional choices. Are there documented moments where Buddhist and Shinto authorities chose to maintain incoherence despite opportunities to clarify or separate?',
    'If primarily inertial: the constraint is a degraded piton sustained by sunk cost. If actively maintained: it is a snare where beneficiaries knowingly extract through the perpetuation of incoherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_inertia_vs_active_maintenance, empirical, 'Is the arrangement sustained by institutional inertia or active beneficiary maintenance?').

omega_variable(
    committer_frame_honji_suijaku_vs_incoherent,
    'Could honji-suijaku monism (kami as manifestations of buddhas/bodhisattvas) be understood as a genuine coherent kernel that this reading simply rejects, rather than as an incoherent bundle? Is the incoherence real or is this reading a hermeneutical choice to deny the coherence offered by honji-suijaku?',
    'Examine whether honji-suijaku successfully integrates kami and buddhas at the theoretical level for practitioners who hold that framework. If practitioners can articulate a single coherent ontology through honji-suijaku, the incoherence may be a reading-specific claim rather than a structural fact. The sibling reading honji_suijaku_monism will argue that coherence is achievable within a single framework.',
    'If honji-suijaku provides genuine theoretical coherence: this reading''s claim to incoherence is a rejection of that framework, not a discovery of structural contradiction. If honji-suijaku is itself contradicted by domain partition in actual practice: the incoherence reading is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_honji_suijaku_vs_incoherent, conceptual, 'Whether incoherence is structural or a hermeneutical judgment about coherence-denying readings.').

omega_variable(
    reformation_attempts_suppression_vs_failure,
    'Were reformation attempts to impose coherence (separation, hierarchy, systematic honji-suijaku) suppressed by institutional power, or did they fail for lack of compelling force (practitioners did not want coherence, incoherence was genuinely preferred)?',
    'Historical evidence: were reformers prevented from speaking (institutional suppression) or did their frameworks fail to convince even when allowed voice? Are there counterhistories where coherence was offered but rejected by ordinary practitioners?',
    'If suppressed: the constraint''s persistence is enforced by power, supporting the tangled_rope/snare reading. If rejected: practitioners may prefer incoherence, which would reframe the constraint as rope-like coordination that solves genuine needs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformation_attempts_suppression_vs_failure, empirical, 'Were coherence-seeking alternatives suppressed or simply unpersuasive?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__incoherent_bundle, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__incoherent_bundle, theater_ratio, 0, 0.25).
narrative_ontology:measurement(kami_tr_t5, kami_buddha_ontology__incoherent_bundle, theater_ratio, 5, 0.35).
narrative_ontology:measurement(kami_tr_t10, kami_buddha_ontology__incoherent_bundle, theater_ratio, 10, 0.45).
narrative_ontology:measurement(kami_tr_t15, kami_buddha_ontology__incoherent_bundle, theater_ratio, 15, 0.54).
narrative_ontology:measurement(kami_tr_t20, kami_buddha_ontology__incoherent_bundle, theater_ratio, 20, 0.57).
narrative_ontology:measurement(kami_tr_t25, kami_buddha_ontology__incoherent_bundle, theater_ratio, 25, 0.58).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(kami_be_t5, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(kami_be_t10, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(kami_be_t15, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(kami_be_t20, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(kami_be_t25, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(kami_su_t5, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(kami_su_t10, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(kami_su_t15, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(kami_su_t20, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(kami_su_t25, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__incoherent_bundle, identity_coordination).
narrative_ontology:boltzmann_floor_override(kami_buddha_ontology__incoherent_bundle, 0.15).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology__honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology__domain_partition).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kami_buddha_ontology kernel. The incoherent_bundle reading denies that any single coherent ontology (monism or partition) is upheld institutionally—instead, shinbutsu-shugo sustains contradictory frameworks simultaneously through institutional power and practical efficacy. The other two readings (honji_suijaku_monism claiming unified ontology, domain_partition claiming systematic functional separation) each assert a coherent framework. This reading argues that institutional practice sustains none of them—the coherence is performative rather than real. All three readings reference the same historical phenomenon but frame it through different epistemic assumptions about what counts as coherence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology__incoherent_bundle, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
