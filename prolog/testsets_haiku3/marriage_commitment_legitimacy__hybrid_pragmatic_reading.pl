% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__hybrid_pragmatic_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: marriage_commitment_legitimacy__hybrid_pragmatic_reading
 *   human_readable: Marriage Commitment Legitimacy (Hybrid Pragmatic Reading)
 *   domain: religious institutional history / political theology
 *
 * SUMMARY:
 *   In 2015, a federal court invalidated a major religious institution's
 *   marriage ban on grounds of equal protection. Institutional leadership
 *   faced an immediate crisis: open defiance of federal authority would
 *   trigger legal consequences and fragment the membership; visible
 *   capitulation would undermine internal doctrinal legitimacy. The Manifesto
 *   resolves this through strategic scope ambiguity: it frames the marriage
 *   reversal as prophetic adaptation rather than doctrinal abandonment,
 *   claiming that 'eternal truths' remain unchanged while 'earthly practice'
 *   accommodates the exogenous crisis. This reading is one of three
 *   structurally distinct interpretations of the Manifesto—each carrying
 *   different implications about whether the constraint represents genuine
 *   theological revelation (endogenous reinterpretation), federal coercion
 *   (exogenous override), or pragmatic institutional adaptation (hybrid
 *   pragmatic). This story instantiates the hybrid pragmatic reading.
 *
 * KEY AGENTS:
 *   - institutional_leadership: agenda-setter; authors the Manifesto reading and enforces it through interpretive authority; benefits from the ambiguity that preserves both compliance and doctrinal autonomy
 *   - rank_and_file_members: payer; identity-locked into the institution; bear the cost of interpretive uncertainty about whether the Manifesto is genuinely prophetic or strategically accommodating
 *   - doctrinal_traditionalists: payer + secondary beneficiary; retain formal doctrinal supremacy but inherit interpretive vulnerability from the scope-ambiguity mechanism
 *   - federal_authority: beneficiary; achieves practical compliance without forcing visible institutional capitulation
 *   - schism_risk_community: excluded; would reject the pragmatic reading but lack resources to sustain schism; their potential defection is what the constraint's ambiguity forestalls
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.58).
domain_priors:suppression_score(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.62).
domain_priors:theater_ratio(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "Marriage Commitment Legitimacy (Hybrid Pragmatic Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "religious institutional history / political theology").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '34b7c522-73a8-433d-ae68-052c49c6821e').
narrative_ontology:cs_kernel_codification('34b7c522-73a8-433d-ae68-052c49c6821e', fixed_text).
narrative_ontology:cs_authority_grounding('34b7c522-73a8-433d-ae68-052c49c6821e', lineage).
narrative_ontology:cs_interpretation_layer_present('34b7c522-73a8-433d-ae68-052c49c6821e').
narrative_ontology:cs_reading_relation('34b7c522-73a8-433d-ae68-052c49c6821e', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('34b7c522-73a8-433d-ae68-052c49c6821e', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('34b7c522-73a8-433d-ae68-052c49c6821e', foundational, prophecy_can_accommodate_exogenous_crisis).
narrative_ontology:cs_axiom_status(prophecy_can_accommodate_exogenous_crisis, holdable).
narrative_ontology:cs_axiom_grounding('34b7c522-73a8-433d-ae68-052c49c6821e', prophecy_can_accommodate_exogenous_crisis, theological).
narrative_ontology:cs_axiom('34b7c522-73a8-433d-ae68-052c49c6821e', foundational, doctrine_eternal_practice_mutable_distinction).
narrative_ontology:cs_axiom_status(doctrine_eternal_practice_mutable_distinction, holdable).
narrative_ontology:cs_axiom_grounding('34b7c522-73a8-433d-ae68-052c49c6821e', doctrine_eternal_practice_mutable_distinction, deontological).
narrative_ontology:cs_reference_frame('34b7c522-73a8-433d-ae68-052c49c6821e', doctrine_unchanging_prophetic_authority).
narrative_ontology:cs_drift_state('34b7c522-73a8-433d-ae68-052c49c6821e', post_federal_override_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('34b7c522-73a8-433d-ae68-052c49c6821e', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, doctrinal_traditionalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, doctrinal_traditionalists).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors and enforces the Manifesto reading, balancing federal legal compliance with preservation of doctrinal authority. Frames the marriage reversal as prophetic adaptation rather than capitulation, preserving institutional legitimacy with the broader theological tradition while accommodating exogenous political pressure. Benefits from maintaining both compliance and interpretive flexibility—the Manifesto becomes a tool for institutional preservation.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Faces interpretive uncertainty about whether the Manifesto represents genuine theological revelation, political accommodation, or some unstable combination. Must either accept the leadership's reading or exit; exit means loss of religious community, identity, and social standing. Bears the cost of legitimacy ambiguity—the constraint's persistence depends on their acceptance of the leadership's framing despite the tension between official doctrine and practice.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members, payer,
    powerless, biographical, identity_locked, national).

% Retain formal doctrinal supremacy through the leadership's claim that the Manifesto preserves rather than abandons core theology. The scope ambiguity—'eternal truths' vs. 'earthly practice'—allows them to hold that doctrine remains unchanged while practice adapts. But this same ambiguity creates permanent interpretive vulnerability: the boundary between doctrine and practice is now contestable, and future reinterpretations become easier to justify.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, doctrinal_traditionalists, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__hybrid_pragmatic_reading, doctrinal_traditionalists, beneficiary).

% Achieves practical compliance (the marriage ban is reversed) without the institution collapsing or claiming outright coercion. The Manifesto's hybrid framing allows the federal government to declare victory while the religious institution preserves enough interpretive autonomy to maintain internal legitimacy and prevent schism.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_authority, beneficiary,
    institutional, generational, analytical, national).

% Traditionalist members who cannot accept the Manifesto's reading and would exit or split but lack the institutional resources to sustain a parallel organization. They are excluded from the leadership table where the reading is negotiated, yet their potential defection is what the constraint's ambiguity is designed to forestall.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, schism_risk_community, excluded,
    organized, generational, trapped, national).

% Academic and religious scholarship examining the Manifesto's legitimacy status. Their analysis can either reinforce the leadership's pragmatic reading or expose it as incoherent, reshaping how the constraint is perceived across the tradition.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, external_theological_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__hybrid_pragmatic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains institutional coherence across federal legal obligation and internal theological authority by deploying scope ambiguity: 'eternal doctrine' remains officially unchanged while 'earthly practice' adapts to exogenous crisis. Solves the problem of institutional survival under conflicting mandates.
% TRANSFER_FUNCTION: Transfers the burden of interpretive uncertainty from institutional leadership to rank-and-file members and traditionalists, who must accept the leadership's framing or bear the cost of exit. Leadership collects the benefit of both federal compliance and retained doctrinal authority without openly choosing between them.
% ABSENT_VOICES: Schism-risk traditionalists and members who would reject the pragmatic reading are structurally excluded from the negotiation table where the reading is authorized. They would testify that the Manifesto represents capitulation, not adaptation; their exclusion is the constraint's hidden premise.
% DISAPPEARANCE_RATIONALE: If the Manifesto and its hybrid framing disappeared, the institution would face immediate crisis: either openly choose federal compliance (losing doctrinal traditionalists), openly resist compliance (triggering legal and political consequences), or fragment into competing readings. The Manifesto's scope ambiguity is what holds the institutional coalition together.
% FOUNDING_PROBLEM: Federal law invalidated the institution's marriage ban in 2015; institutional leadership faced immediate pressure to either capitulate visibly (risking internal legitimacy) or openly resist (risking legal/political consequences). The founding problem is the institutional crisis created by the exogenous federal override of longstanding doctrine.
% FOUNDING_PROBLEM_CORROBORATION: Institutional leadership and federal authorities both attest the problem was live and urgent in 2015–2019. Historical and religious scholarship from outside the benefiting parties documents the doctrinal and legal conflict; observers note that institutional fragmentation did occur among member communities and schismatic groups, confirming the stakes of the leadership's response.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58 at interval end) because the leadership collects both federal compliance and doctrinal autonomy while deflecting the cost of ambiguity to rank-and-file members. The measurement series show extractiveness rising sharply in the first 6 time points (0.42→0.53) as the Manifesto's implications become clearer, then plateauing (0.53→0.58) as institutional members either internalize the reading or exit. Suppression is high (0.62) because the constraint's persistence depends on actively suppressing alternative readings—schismatic interpretations must be delegitimized, and internal doctrinal review must remain controlled by leadership. Theater ratio rises from 0.25 to 0.48 and plateaus, indicating that increasing shares of the enforcement activity are devoted to maintenance of the ambiguity itself rather than the original doctrinal function. All three metrics are authored on a single shared time grid (every metric at every time point), per the OQ-105 alignment rule. The claim/metric gap is deliberate: tangled_rope implies genuine coordination function (institutional survival under conflicting mandates) + asymmetric extraction (leadership benefits, members bear cost). The hybrid pragmatic reading emphasizes the coordination aspect—avoiding schism is real—but the authored metrics capture the extraction component (the scope ambiguity is maintained through suppression, not consent).
 *
 * PERSPECTIVAL GAP:
 *   The institutional leadership's seat perceives this as rope: genuine coordination solving an acute institutional crisis through prophetic flexibility. The rank-and-file members' seat perceives it as snare: a mechanism for extracting their consent and interpretive compliance without offering them a choice. The schism-risk community perceives it as extraction pure: the Manifesto's ambiguity exists to prevent their exit, not to solve any coordination problem they recognize. The engine computes each seat's perceived type from the power/exit/beneficiary data; this story's structural data should produce divergent classifications—leadership near rope, members and traditionalists nearer tangled_rope or snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership holds institutional power and arbitrage-grade exit (can reinterpret doctrine and adapt to political change); they are a beneficiary, d near 0.2–0.3. Rank-and-file members hold no power and identity-locked exit (losing membership means losing religious identity and community); they are victims, d near 0.75–0.85. Doctrinal traditionalists hold moderate power (organized, credible resistance potential) but constrained exit (leaving means schism risk); they are victims of the scope ambiguity mechanism, d near 0.65–0.75. Federal authority holds institutional power but analytical exit (can only observe outcomes); they are a beneficiary, d near 0.1–0.2. The directionality structure explains the tangled_rope classification: leaders and federal authority benefit from coordination without bearing its cost; members and traditionalists pay through interpretive uncertainty and exit constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal law overrides institutional doctrine) is live at the founding but contested at the interval end. The Manifesto's scope ambiguity creates a situation where neither the endogenous reading (God commanded the reversal) nor the exogenous reading (federal coercion forced it) can be falsified—the ambiguity itself becomes the institutional commitment. This prevents mandatrophy in the classical sense (founding problem becomes dead but constraint persists). However, it creates a different kind of drift: the constraint's justification shifts from the coordination problem (preventing schism) to the extraction mechanism (maintaining ambiguity for its own sake). Theater ratio rising to 0.48 suggests that by t=20, nearly half of enforcement activity is divorced from the founding coordination function. This is not mandatrophy (the problem is still live), but it is extractive drift—the mechanism persists beyond its justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prophetic_authenticity_ambiguity,
    'Is the Manifesto''s claim to prophetic authority genuine theological revelation, or strategic institutional framing of federal accommodation?',
    'Theological tradition-internal analysis: does the Manifesto cohere with prior prophetic claims in the institution? External observer testimony: do scholars outside the institution perceive the mechanism as prophetic adaptation or transparent capitulation? Ethnographic study: do rank-and-file members report perceiving the Manifesto as divinely commanded or institutionally expedient?',
    'If resolved as genuine prophecy, the constraint should be reclassified as rope or even mountain (divine command is binding, coordination function is real, extraction is incidental). If resolved as institutional framing, the classification remains tangled_rope or slides toward snare (the prophecy claim is cover, extraction is the primary mechanism).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prophetic_authenticity_ambiguity, conceptual, 'Whether the Manifesto is genuine prophetic revelation or institutional strategic adaptation.').

omega_variable(
    scope_ambiguity_sustainability,
    'Can the boundary between ''eternal doctrine'' and ''earthly practice'' be maintained indefinitely as a stable institutional commitment, or will future reinterpretations collapse the distinction and force explicit choice?',
    'Historical trajectory: measure rate of subsequent doctrinal reinterpretations. If further practice reversals claim the same ''scope preservation'' mechanism, the distinction becomes a precedent for continuous doctrinal adjustment; if the institution actively resists new scope claims, the boundary becomes a one-time exception. Institutional record: examine leadership statements and theological commentary to identify when the scope boundary is treated as fixed vs. revisable.',
    'If the distinction collapses, the hybrid pragmatic mechanism fails and the constraint moves toward explicit exogenous_override_reading (federal pressure broke doctrine, adaptation was capitulation). If the distinction holds, the hybrid reading remains viable and the constraint persists as tangled_rope. The resolution also determines whether doctrinal traditionalists can maintain ''doctrine unchanged'' or are forced to acknowledge doctrine has begun to shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_ambiguity_sustainability, empirical, 'Whether scope ambiguity can sustain institutional legitimacy long-term or will eventually collapse.').

omega_variable(
    identity_lock_mechanism_specificity,
    'What specific identity-fusion mechanism binds rank-and-file members to the institution despite the Manifesto''s interpretive ambiguity? Is it career path dependence (spiritual vocation), relational identity (community membership), ideological identity (worldview constituted by doctrine), or institutional identity (the organization ''is'' the tradition)?',
    'Exit cohort analysis: which members exit despite identity-lock, and what reasons do they cite? Retention interview data: among those who stay, which framing (career, relational, ideological, institutional) most resists exit? Historical comparison: how did identity-lock mechanisms function before the Manifesto, and what changed?',
    'If identity-lock is primarily career-based, members have secondary exit routes (seminary re-credentialing outside the institution); if relational, loss of community is the primary cost; if ideological, the Manifesto''s ambiguity is most damaging (worldview is now incoherent); if institutional, members have the deepest lock (the organization is their identity). The tightness of identity-lock explains why suppression can be moderate (0.62) rather than extremely high: members are more self-suppressing than externally suppressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_specificity, empirical, 'What type of identity-fusion mechanism binds members despite interpretive ambiguity.').

omega_variable(
    reading_coexistence_vs_foreclosure,
    'Can the three readings (endogenous, exogenous, hybrid pragmatic) genuinely coexist within the institutional framework, or does adoption of one reading logically foreclose the others?',
    'Examine institutional leadership communication: are all three readings presented as live options, or is one reading treated as the authoritative truth? Theological analysis: does the endogenous reading (genuine prophecy) logically exclude the exogenous reading (coercion), or can an institution hold both (God worked through the federal court)? Schismatic analysis: which reading did defecting groups adopt, and does it show that institutional members experienced the readings as mutually exclusive?',
    'If the readings coexist, the institutional commitment is genuinely open and the constraint operates as tangled_rope (ambiguity is maintained, multiple readings cohere). If one reading forecloses others, the institution is not transparent about which reading it endorses, and the constraint becomes more snare-like (members are expected to hold a specific reading they may not privately endorse). This affects the classification at different seats: what is rope to leadership (multiple readings possible) may be snare to members (only one reading permitted).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_vs_foreclosure, conceptual, 'Whether the three Manifesto readings genuinely coexist or whether one forecloses others.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t3, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 3, 0.33).
narrative_ontology:measurement_basis(marr_tr_t3, observed).
narrative_ontology:measurement(marr_tr_t6, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 6, 0.4).
narrative_ontology:measurement_basis(marr_tr_t6, observed).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 10, 0.46).
narrative_ontology:measurement_basis(marr_tr_t10, observed).
narrative_ontology:measurement(marr_tr_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement_basis(marr_tr_t15, observed).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(marr_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t3, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement_basis(marr_be_t3, observed).
narrative_ontology:measurement(marr_be_t6, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 6, 0.53).
narrative_ontology:measurement_basis(marr_be_t6, observed).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement_basis(marr_be_t10, observed).
narrative_ontology:measurement(marr_be_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(marr_be_t15, observed).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(marr_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t3, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 3, 0.59).
narrative_ontology:measurement_basis(marr_su_t3, observed).
narrative_ontology:measurement(marr_su_t6, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 6, 0.61).
narrative_ontology:measurement_basis(marr_su_t6, observed).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(marr_su_t10, observed).
narrative_ontology:measurement(marr_su_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(marr_su_t15, observed).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(marr_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__hybrid_pragmatic_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.12).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The marriage_commitment_legitimacy kernel decomposes into three constraint stories, each instantiating a different reading of the Manifesto. The hybrid_pragmatic_reading (this story) treats the Manifesto as strategic institutional adaptation using scope ambiguity to preserve both federal compliance and doctrinal authority. The endogenous_reinterpretation_reading treats it as genuine prophetic revelation; the exogenous_override_reading treats it as coerced federal capitulation. Each reading assigns different ε, beneficiary/victim structures, and computed per-seat types. The three stories are linked via network.affects_constraints: the pragmatic reading influences both siblings by establishing the Manifesto's institutional function (scope management). All three are epistemically live and coexist within institutional and scholarly discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
