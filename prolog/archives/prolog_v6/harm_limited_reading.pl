% ============================================================================
% CONSTRAINT STORY: harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_harm_limited_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: harm_limited_reading
 *   human_readable: Speech Protection Conditional on Absence of Significant Harm (Harm-Limited Reading)
 *   domain: constitutional_law/speech_regulation/political_philosophy
 *
 * SUMMARY:
 *   The harm-limited reading of speech protection defines the protected
 *   category as conditional on absence of significant harm to dignity,
 *   equality, and freedom from harassment. This reading instantiates one of
 *   three structurally distinct commitments to the speech-protection kernel:
 *   the absolutist reading (speech protection is categorically prior; harm is
 *   addressed via non-speech law), the balancing reading (speech and harm
 *   prevention are co-primary; courts weigh contextually), and this
 *   harm-limited reading (dignity and equality are foundational; speech
 *   protection is derivative). The harm-limited reading experiences the
 *   constraint as a genuine coordination mechanism (protecting people from
 *   dignitary harm) with embedded extraction (state gains regulatory
 *   discretion; speech targets face legal uncertainty; enforcement becomes
 *   selective). This is a canonical tangled_rope structure: real coordination
 *   benefit exists (harassment does cause measurable harm), but the mechanism
 *   requires active state enforcement that creates new extraction vectors
 *   (prosecutorial discretion, selective application, identity-based
 *   enforcement patterns).
 *
 * KEY AGENTS:
 *   - Marginalized Dignity-Claimants: Primary beneficiaries (powerless/trapped to moderate/constrained) — seek remedy for harassment and hate speech; gain protection framework alongside exposure to state gatekeeping
 *   - State Regulatory Authority: Primary beneficiary (institutional/arbitrage) — expands interpretive and enforcement power; gains legitimacy from dignity protection mandate; experiences constraint as coordinative
 *   - Boundary-Zone Speech Targets: Primary victims (powerless/trapped) — utterances in gray zones (ambiguous dog whistles, contextual offense, coded harassment) face legal vulnerability without clear protection; chilling effect; state discretion
 *   - Dominant Speech Communities: Secondary actor (powerful/mobile) — experience mixed tangled_rope effects: constrained mobility for offensive speech, but mobile enough to navigate standards or relocate speech venues
 *   - Transnational Civil Society: Organized actors (organized/constrained) — treat the reading as a transitional scaffold; build infrastructure for harm-limitation but with sunset orientation toward counter-speech maturity
 *   - Administrative Law Apparatus: Institutional inertia (institutional/arbitrage) — maintains performative enforcement; selective application; degraded ritual (piton dynamic)
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing equality-first commitment as universal human law rather than recognizing it as a particular reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(harm_limited_reading, 0.58).
domain_priors:suppression_score(harm_limited_reading, 0.62).
domain_priors:theater_ratio(harm_limited_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(harm_limited_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(harm_limited_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(harm_limited_reading, theater_ratio, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(harm_limited_reading, "Speech Protection Conditional on Absence of Significant Harm (Harm-Limited Reading)").
narrative_ontology:topic_domain(harm_limited_reading, "constitutional_law/speech_regulation/political_philosophy").

domain_priors:requires_active_enforcement(harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(harm_limited_reading, '14e29d82-4df6-4d63-93d2-0dfb0dc03556').
narrative_ontology:cs_created_at('14e29d82-4df6-4d63-93d2-0dfb0dc03556', '').
narrative_ontology:cs_kernel_codification('14e29d82-4df6-4d63-93d2-0dfb0dc03556', formalized).
narrative_ontology:cs_authority_grounding('14e29d82-4df6-4d63-93d2-0dfb0dc03556', lineage).
narrative_ontology:cs_interpretation_layer_present('14e29d82-4df6-4d63-93d2-0dfb0dc03556').
narrative_ontology:cs_kernel_id(harm_limited_reading, speech_protection_boundary).
narrative_ontology:cs_reading_relation('14e29d82-4df6-4d63-93d2-0dfb0dc03556', absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('14e29d82-4df6-4d63-93d2-0dfb0dc03556', balancing_reading, coexists_with).
narrative_ontology:cs_axiom('14e29d82-4df6-4d63-93d2-0dfb0dc03556', foundational, equal_dignity_foundational).
narrative_ontology:cs_axiom_status(equal_dignity_foundational, holdable).
narrative_ontology:cs_axiom_grounding('14e29d82-4df6-4d63-93d2-0dfb0dc03556', equal_dignity_foundational, deontological).
narrative_ontology:cs_axiom('14e29d82-4df6-4d63-93d2-0dfb0dc03556', foundational, harm_limitation_empirically_protective).
narrative_ontology:cs_axiom_status(harm_limitation_empirically_protective, holdable).
narrative_ontology:cs_axiom_grounding('14e29d82-4df6-4d63-93d2-0dfb0dc03556', harm_limitation_empirically_protective, empirically_contingent).
narrative_ontology:cs_reference_frame('14e29d82-4df6-4d63-93d2-0dfb0dc03556', equal_dignity_foundational_commitment).
narrative_ontology:cs_drift_state('14e29d82-4df6-4d63-93d2-0dfb0dc03556', contemporary_regulatory_expansion, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(harm_limited_reading, marginalized_dignity_holders).
narrative_ontology:constraint_beneficiary(harm_limited_reading, equality_enforcement_constituencies).
narrative_ontology:constraint_victim(harm_limited_reading, speech_boundary_uncertainty).
narrative_ontology:constraint_victim(harm_limited_reading, state_regulatory_discretion_targets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETS OF REGULATED MARGINAL SPEECH (SNARE) — Those whose speech falls into the boundary zone (ambiguous dog whistles, contextual harassment, coded offense) face maximal suppression. Cannot appeal to bright-line protection; must navigate state discretion; bear extraction cost of legal uncertainty and chilling effect. No exit option except silence.
constraint_indexing:constraint_classification(harm_limited_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DIGNITY-CLAIMING CONSTITUENCY (TANGLED ROPE) — Those seeking protection from harassment and hate speech experience genuine coordination benefit (remedy for dignitary harm) alongside embedded extraction: must prove harm severity, navigate harm standards, accept state gatekeeping, bear costs of proof burdens. Mixed experience — real protection but with asymmetric application and state discretion.
constraint_indexing:constraint_classification(harm_limited_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE ENFORCEMENT AUTHORITY (ROPE) — Interprets and enforces harm standards; gains legitimacy from protecting dignity and equality; experiences constraint as primarily coordinative (solving collective action problem of unregulated harassment). Net beneficiary via authority expansion and institutional discretion, but frames expansion as coordination. Low or negative experienced extraction because benefits of authority accrue directly to institutional position.
constraint_indexing:constraint_classification(harm_limited_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TRANSNATIONAL CIVIL SOCIETY NETWORKS (SCAFFOLD) — International human rights frameworks (ICCPR Article 20, hate speech protocols) create temporary institutional bridges between speech restriction and human dignity protection. These networks see the harm-limited reading as a transitional coordination mechanism with sunset logic: as norms of civil discourse mature and counter-speech capacity strengthens, formal speech restriction becomes less necessary. Sunset estimated at 30-50 years as internet literacy and digital counter-speech infrastructure mature.
constraint_indexing:constraint_classification(harm_limited_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: ADMINISTRATIVE LAW APPARATUS (PITON) — The existing regulatory infrastructure for speech (FCC decency standards, platform content moderation, hate speech prosecution) has largely atrophied into theater: standards are applied inconsistently, enforcement is selective, appeals are expensive and slow, and the actual speech change achieved is marginal relative to procedural costs. The system persists through institutional inertia and legitimacy claims rather than functional verification. Theater ratio is moderate because some genuine harm prevention occurs, but much is ceremonial compliance.
constraint_indexing:constraint_classification(harm_limited_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DOMINANT SPEECH COMMUNITIES (TANGLED ROPE) — Those with structural speech privilege experience coordination benefits (reduced harassment harassment, maintained social capital) alongside constrained mobility: cannot rely on unvetted offensive speech without reputational cost, face elevated scrutiny for coded utterances, must navigate expanding interpretive standards. Benefits and extraction are genuinely mixed — some loss of expressive freedom, but for powerful speakers this is a constrained cost, not trapped suppression. At biographical horizon, this perspective shifts from rope to tangled rope: the harm standard restricts elite speech more than initially apparent.
constraint_indexing:constraint_classification(harm_limited_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, the harm-limited reading appears to rest on an irreducible empirical and normative foundation: human dignity and freedom from targeted degradation are universal constraints on coexistence. The reading frames the harm standard as a natural law of human social organization — dignity cannot be conditional on others' speech freedom; therefore speech that destroys dignity cannot enjoy protection. This perspective risks false summit classification: naturalizing what is actually a particular reading of how to balance competing commitments.
constraint_indexing:constraint_classification(harm_limited_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(harm_limited_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(harm_limited_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(harm_limited_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(harm_limited_reading, TR),
    TR >= 0.70.

:- end_tests(harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reading creates genuine coordination benefit (protection from dignitary harm) but at the cost of expanded state interpretive power. The 0.58 value reflects that the extraction is real and significant (state discretion in boundary-setting, selective prosecution patterns, chilling effect on marginal speech) but not maximal — the coordination function is genuine and the beneficiaries (dignity-claimants) do experience protection, not pure extraction. Theater ratio (0.51): Moderate. The harm-limited reading relies on measurable harm standards, but in practice, harm determinations are often contested and enforcement is selective. The moderate theater reflects that some genuine harm-prevention occurs but much administrative activity is performative (due-process compliance, regulatory theater without behavioral change). Suppression (0.62): High. Those whose speech falls into the boundary zone face significant barriers: legal uncertainty about whether an utterance crosses the harm threshold, chilling effect discouraging marginal speech, asymmetric prosecution patterns, expensive appeals. Suppression is not total (legal remedy exists), but it is substantial. Claimed type (tangled_rope): Justified by the combination of genuine coordination function (harm prevention) + asymmetric extraction (state discretion, selective enforcement) + active enforcement requirement. The reading cannot be classified as pure rope (no extraction) or pure snare (no coordination); both functions are real.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces significant perspectival gaps that reveal the kernel contest. The state authority sees primarily rope — coordination of harm prevention, authority to adjudicate boundaries, legitimate gatekeeping. The dignity-claimants see primarily rope/tangled_rope — protection gained (rope) but through state discretion (tangled_rope). The boundary-zone speech targets see snare — legal vulnerability without clear rules, chilling effect, suppression. The analytical observer risks seeing mountain (universal commitment to equal dignity) when the actual structure is a contested reading. The gap between rope (state authority), tangled_rope (dignity-claimants), and snare (boundary-zone targets) reveals that the reading's coordinative function is experienced very differently depending on structural position. No single classification fits all perspectives — the presheaf over positions is the accurate model. The false summit risk (analytical seeing mountain) is particularly acute because the harm-limited reading explicitly claims foundation in universal human dignity, but this claim is actually a particular normative priority choice that the absolutist and balancing readings contest.
 *
 * DIRECTIONALITY LOGIC:
 *   The reading's directionality profile reflects its structural ambivalence: beneficiaries (marginalized dignity-claimants) are typically lower-power actors experiencing trapped or constrained exit options; state authority is higher-power institutional actor with arbitrage options; boundary-zone speech targets are powerless/trapped. The constraint creates different d values for each agent: dignity-claimants get beneficiary status (low/negative d) but also trapped exit (high d) — partially offsetting. State authority gets beneficiary status + arbitrage exit (very negative d). Boundary-zone speech targets get victim status + trapped exit (highest d). The reading's extraction mechanism is concentrated on those least able to exit — precisely the pattern tangled_rope requires. The analytics perspective at civilizational horizon applies canonical d for analytical (0.73) but the perspective risks false summit: it naturalizes the equality-first commitment as universal law rather than recognizing it as one reading among contested alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   The harm-limited reading resolves mandatrophy by recognizing that the speech-protection kernel admits multiple legitimate readings, each with different extraction structures. The reading is not claiming that speech protection is only tangled_rope; rather, it is claiming that *this particular commitment* (dignity-as-foundational) produces a tangled_rope structure when enforced. The absolutist reading would produce rope or piton (minimal extraction, speech protection as categorical). The balancing reading would produce tangled_rope but with different beneficiary/victim distributions. The reading's own classification is tangled_rope because it commits to harm-limitation while also requiring state enforcement that inevitably produces discretion and selective application. The resolution is not 'which reading is correct?' but 'which reading do you commit to?' — and the answer has structural consequences for who bears extraction and who benefits from coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_severity_threshold_indeterminacy,
    'What constitutes ''significant harm'' to dignity and equality? Where does contextual offense end and dignitary harm begin?',
    'Empirical studies on psychological and social harm from hate speech and harassment; correlation with measurable dignity metrics (self-reported dignity, social participation, economic opportunity); cross-jurisdictional comparative analysis of harm standards and outcomes',
    'If threshold is strict (extreme harm only): constraint reclassifies toward rope, state gatekeeping power recedes, most dog whistles remain unprotected. If threshold is loose (context-dependent offense): constraint remains tangled_rope or shifts toward snare, state discretion expands, many marginal utterances become regulable. Classification cascades from this resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_severity_threshold_indeterminacy, empirical, 'Determination of what constitutes significant harm to dignity or equality').

omega_variable(
    state_abuse_risk_vs_protection_gain,
    'Do harm-based speech restrictions, in practice, reduce net harm to dignity and equality, or do state enforcement abuses exceed the harm prevented?',
    'Longitudinal analysis of jurisdictions implementing harm-limited speech restrictions: measure dignitary harm (hate crimes, harassment incidents, discrimination markers) before/after; cross-correlate with state prosecution rates, conviction patterns, and documented prosecutorial abuse or selective enforcement; compare to jurisdictions with absolutist speech regimes on same dignity metrics',
    'If protection gains > abuse costs: constraint classification (tangled_rope) is empirically justified. If abuse costs > protection gains: constraint reclassifies as snare (extraction masked by legitimacy claim) or toward piton (performative enforcement without real benefit). This is the core reading''s empirical wager.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_abuse_risk_vs_protection_gain, empirical, 'Whether harm-based restrictions reduce net harm or amplify state abuse').

omega_variable(
    reading_contest_empirical_contingency,
    'Does this harm-limited reading''s empirical premise hold, or is dignity protection more reliably achieved through absolutist speech rights + robust counter-speech + anti-discrimination law outside speech regulation?',
    'Comparative institutional analysis: measure dignity and equality outcomes (representation, trust, discrimination, violence) across three regime types: (a) harm-limited speech restriction + weak anti-discrimination law, (b) absolutist speech + strong anti-discrimination law, (c) harm-limited speech + strong anti-discrimination law. Control for wealth, institutional capacity, and democratic accountability.',
    'This omega resolves the core theoretical disagreement between this reading and the absolutist sibling: whether dignity is better protected via speech regulation or via non-speech legal remedies. If (b) or (c) outperform (a), the harm-limited reading''s foundational axiom is empirically challenged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_empirical_contingency, empirical, 'Empirical validity of harm-limited reading''s protective mechanism').

omega_variable(
    kernel_reading_contest_commitment_frame,
    'Is the speech-protection kernel fundamentally a commitment to individual liberty, or a commitment to equal dignity in shared spaces? Different frames produce different readings.',
    'Analysis of the kernel''s canonical texts (1A jurisprudence, universal declarations, founding-era intent) to identify which commitment is foundational. If liberty-first: absolutist reading is more coherent. If equality-first: harm-limited reading is more coherent. If ambiguous: balancing reading emerges as default.',
    'This is a conceptual omega: it maps to the choice of reference_frame in cs_structure. The harm-limited reading commits to equality-as-foundational; absolutist commits to liberty-as-foundational. No empirical resolution is possible — the reading choice enacts a particular normative priority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_commitment_frame, conceptual, 'Which commitment (liberty vs equality) is foundational to the speech-protection kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(harm_limited_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(harm_tr_t0, harm_limited_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(harm_tr_t10, harm_limited_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(harm_tr_t20, harm_limited_reading, theater_ratio, 20, 0.51).

% Extraction over time
narrative_ontology:measurement(harm_be_t0, harm_limited_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(harm_be_t10, harm_limited_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(harm_be_t20, harm_limited_reading, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(harm_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(harm_limited_reading, absolutist_reading).
narrative_ontology:affects_constraint(harm_limited_reading, balancing_reading).

% DUAL FORMULATION NOTE:
% The harm-limited reading is one of three structurally distinct readings of the speech-protection kernel. Each reading has its own constraint_id, its own ε value, its own beneficiary/victim structure. The absolutist_reading (ε ≈ 0.15, rope/mountain) prioritizes speech as foundational. The balancing_reading (ε ≈ 0.42, tangled_rope) treats speech and harm as co-primary. This harm-limited reading (ε = 0.58, tangled_rope) prioritizes dignity/equality. They form a reading family linked by network.affects_constraints. The ε values differ because the readings have different enforcement structures and beneficiary/victim distributions — not because one is 'more accurate' than the others, but because they enact different normative commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(harm_limited_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
