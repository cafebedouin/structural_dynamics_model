% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__competitive_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article17_erasure_right__competitive_moat_reading, []).

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
 *   constraint_id: article17_erasure_right__competitive_moat_reading
 *   human_readable: Article 17 GDPR Erasure Right as Competitive Moat via Compliance Cost Asymmetry
 *   domain: technology/legal/competition
 *
 * SUMMARY:
 *   Article 17 of the GDPR grants individuals the right to erasure ('right to
 *   be forgotten'). This constraint instantiates one specific reading of that
 *   kernel: Article 17 functions as incumbent protection via compliance cost
 *   asymmetry. The reading does not deny that erasure rights serve privacy
 *   interests — it asserts that the specific implementation (universal
 *   erasure obligation, no exemption for competitive burden) operates
 *   secondarily as a competitive filter that raises barriers to entry in
 *   European data markets. Incumbents have already built compliant
 *   infrastructure; entrants must replicate it from day one. This reading
 *   sits in structural tension with the privacy_fundamental_reading (which
 *   centers erasure as individual empowerment) and the
 *   censorship_mechanism_reading (which views erasure as a tool for
 *   weaponized content suppression). All three are live readings of the same
 *   Article 17 kernel, instantiated by different parties in the same legal
 *   framework.
 *
 * KEY AGENTS:
 *   - incumbent_platform_operators: institutional power; arbitrage exit; benefits from infrastructure moat
 *   - startup_data_services: moderate power; constrained exit; bears fixed compliance cost with no amortization pathway
 *   - emerging_market_entrants: powerless; trapped; entry barrier is absolute at early stage
 *   - data_subject_individuals: powerless; identity_locked; hold formal right but lack institutional capacity to exercise it independently
 *   - large_data_processors: institutional power; arbitrage exit; capture secondary rents from compliance burden
 *   - regulatory_authorities: institutional; analytical exit; observe compliance but lack remedial authority over competitive effects
 *   - alternative_privacy_frameworks: powerful but excluded; trapped in global floor-setting dynamic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__competitive_moat_reading, 0.68).
domain_priors:suppression_score(article17_erasure_right__competitive_moat_reading, 0.42).
domain_priors:theater_ratio(article17_erasure_right__competitive_moat_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, theater_ratio, 0.51).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__competitive_moat_reading, tangled_rope).
narrative_ontology:human_readable(article17_erasure_right__competitive_moat_reading, "Article 17 GDPR Erasure Right as Competitive Moat via Compliance Cost Asymmetry").
narrative_ontology:topic_domain(article17_erasure_right__competitive_moat_reading, "technology/legal/competition").

domain_priors:requires_active_enforcement(article17_erasure_right__competitive_moat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__competitive_moat_reading, 'd2f4f760-2a3f-4727-b984-1f5b54d04ea0').
narrative_ontology:cs_kernel_codification('d2f4f760-2a3f-4727-b984-1f5b54d04ea0', formalized).
narrative_ontology:cs_authority_grounding('d2f4f760-2a3f-4727-b984-1f5b54d04ea0', lineage).
narrative_ontology:cs_interpretation_layer_present('d2f4f760-2a3f-4727-b984-1f5b54d04ea0').
narrative_ontology:cs_reading_relation('d2f4f760-2a3f-4727-b984-1f5b54d04ea0', article17_erasure_right__privacy_fundamental_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2f4f760-2a3f-4727-b984-1f5b54d04ea0', article17_erasure_right__censorship_mechanism_reading, influences).
narrative_ontology:cs_axiom('d2f4f760-2a3f-4727-b984-1f5b54d04ea0', foundational, erasure_compliance_as_market_structure).
narrative_ontology:cs_axiom_status(erasure_compliance_as_market_structure, holdable).
narrative_ontology:cs_axiom_grounding('d2f4f760-2a3f-4727-b984-1f5b54d04ea0', erasure_compliance_as_market_structure, instrumental).
narrative_ontology:cs_axiom('d2f4f760-2a3f-4727-b984-1f5b54d04ea0', foundational, infrastructure_cost_differentiates_competitive_capacity).
narrative_ontology:cs_axiom_status(infrastructure_cost_differentiates_competitive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('d2f4f760-2a3f-4727-b984-1f5b54d04ea0', infrastructure_cost_differentiates_competitive_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('d2f4f760-2a3f-4727-b984-1f5b54d04ea0', equal_compliance_burden_framework).
narrative_ontology:cs_drift_state('d2f4f760-2a3f-4727-b984-1f5b54d04ea0', contemporary_market_stratification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d2f4f760-2a3f-4727-b984-1f5b54d04ea0', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__competitive_moat_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, incumbent_platform_operators).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, large_data_processors).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, startup_data_services).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, emerging_market_entrants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, data_subject_individuals).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, data_subject_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Already possess distributed technical infrastructure, data retention architectures built for compliance, legal teams specialized in erasure request handling, and documented compliance procedures. They benefit from Article 17 because startups cannot replicate this infrastructure at scale. As the reading's dominant institutional actor, they have shaped erasure compliance standards through their market implementation and influence regulatory interpretation.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, incumbent_platform_operators, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__competitive_moat_reading, incumbent_platform_operators, agenda_setter).

% Must implement full erasure capabilities from day one to legally operate in EU markets, even at minimal scale. They face the fixed cost of building compliant data architectures, hiring compliance experts, and maintaining documentation systems — costs that do not amortize at early revenue levels. Their options are absorbing the cost (destroying unit economics), delaying EU market entry, or building over-engineered systems that waste resources.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, startup_data_services, payer,
    moderate, biographical, constrained, global).

% Face identical compliance costs to incumbents but lack the institutional capacity, legal resources, and architectural legacy to absorb them. Entry barriers are highest for non-Western entrants and companies without pre-existing EU presence. They are trapped: European data markets are essential for scale, but compliance costs price them out; exiting EU scope means ceding the market entirely.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, emerging_market_entrants, payer,
    powerless, biographical, trapped, global).

% Hold the formal legal right to erasure; their identities are enmeshed with their data records and relationship to digital services. They depend on exercising this right through institutional channels they do not control. They experience identity lock because opting out of digital services is economically impossible (financial access, employment, social participation require digital platforms). The right exists but the structural capacity to exercise it independently is absent.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, data_subject_individuals, beneficiary,
    powerless, immediate, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__competitive_moat_reading, data_subject_individuals, payer).

% Enterprise data infrastructure providers (cloud platforms, database vendors, compliance-as-a-service providers) capture secondary rents from incumbent compliance investments. They sell specialized erasure tooling, data deletion verification, retention management platforms. The compliance burden the regulation creates is a profit center for them.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, large_data_processors, beneficiary,
    institutional, generational, arbitrage, global).

% Enforce Article 17 through investigation, fines, and compliance verification. They observe the pattern of compliance-cost stratification but lack authority or mandate to remedy competitive effects. Their role is limited to monitoring whether erasure requests are honored, not whether the compliance infrastructure is proportionate or competitively neutral.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Regulatory regimes outside the EU (e.g., California Consumer Privacy Act, India's data protection framework) adopt different erasure thresholds and compliance models. They are excluded from shaping the EU reading of Article 17 and are trapped in a global market where the most restrictive (EU) standard sets the floor for multinational operations.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, alternative_privacy_frameworks, excluded,
    powerful, generational, trapped, global).

% Monitor Article 17 implementation from the privacy-rights perspective. They observe the compliance burden but are structurally committed to defending the erasure right as a fundamental protection, even when the same right concentrates competitive advantage. Their observational seat creates tension: defending privacy may inadvertently entrench market power.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, consumer_advocacy_organizations, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__competitive_moat_reading, incumbent_platform_operators).
narrative_ontology:fixing_cost_class(article17_erasure_right__competitive_moat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Article 17 solves a genuine collective problem: individuals need a mechanism to remove their data from corporate systems and reclaim informational autonomy. Without a mandate, platform operators could retain indefinitely, accumulating records that follow individuals across platforms and services. The coordination is between data subjects (demand for deletion) and data controllers (obligation to comply).
% TRANSFER_FUNCTION: Moves compliance cost burden from uniformly distributed across all market participants to disproportionately borne by entrants and smaller actors. Redistributes competitive advantage to those who can amortize infrastructure cost across large user bases and long time horizons. Also transfers rent-extraction opportunity to compliance-tool vendors and infrastructure providers who capture secondary extraction from the compliance burden itself.
% ABSENT_VOICES: Startup founders, emerging-market entrepreneurs, and developers in countries where EU-scale infrastructure is not economically buildable from startup stage. They are excluded from shaping erasure compliance standards because they lack institutional voice in EU regulatory processes and market power to influence platform implementations. Competing privacy models (proportionality-based, tiered-by-entity-size, asymmetric-burden frameworks) are not represented in the current design.
% DISAPPEARANCE_RATIONALE: If Article 17 and its compliance infrastructure vanished, European data markets would reorganize within months: startups would enter without the infrastructure burden, alternative erasure mechanisms (contractual, market-driven) might emerge, and competitive intensity in data services would increase. Incumbents would lose the moat; data processing would likely shift to lower-compliance-cost models.
% FOUNDING_PROBLEM: Article 17 was drafted to address corporate data retention as a privacy harm: platforms accumulated lifetime records on individuals, linked across services, creating asymmetric power where corporations know vastly more about users than users know about themselves. The right to erasure was intended to restore informational balance and individual control.
% FOUNDING_PROBLEM_CORROBORATION: Privacy advocates and data-subject rights groups attest the founding problem is still live: data retention continues to exceed user expectations and regulatory limits are actively tested by platforms. Competition policy researchers and startup advocates attest the founding problem (excessive retention) is substantially addressed by market pressure and existing mechanisms (data minimization directives, storage cost incentives) and the current compliance infrastructure now serves a secondary extractive function. European Commission impact assessments document the founding problem; independent economic analyses from outside the benefiting incumbents document the competitive stratification effect.
narrative_ontology:disappearance_verdict(article17_erasure_right__competitive_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__competitive_moat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__competitive_moat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article17_erasure_right__competitive_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__competitive_moat_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article17_erasure_right__competitive_moat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article17_erasure_right__competitive_moat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article17_erasure_right__competitive_moat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 to 0.68 over the interval because compliance capacity concentrates in incumbent hands while entry barriers harden. Early measurements reflect regulatory uncertainty and low effective barrier (years 0-3); mid-interval (years 6-12) reflects normalization of compliance as standard infrastructure and data-deletion-verification markets maturing; later measurements (years 18-25) stabilize at high extractiveness as the landscape calcifies and smaller entrants exit or never enter. Theater ratio rises sharply (0.22→0.51) and then plateaus, indicating that incumbent compliance activity transitions from genuine erasure-request processing to competitive signaling and infrastructure maintenance — they perform compliance sophistication to signal competitive strength, not to minimize actual deletion burden. Suppression requirement stays moderate (0.28→0.42) because the constraint is not maintained by coercive external force — it is maintained by regulatory obligation (Article 17 exists in statute); suppression is the regulatory infrastructure cost to keep entrants from structuring workarounds (data localization, tiered deletion, conditional compliance). The measurement grid is uniform: all three metrics share the same six time points across the 0-25 interval.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent institutional seat, Article 17 is a necessary privacy protection requiring proportionate compliance infrastructure they have already built — their view computes the constraint as rope or mild tangled_rope (coordination cost with some extraction edge). From the startup payer seat, the same statute is a fixed-cost barrier they cannot afford to replicate — their view computes it as snare (pure extraction via infrastructure lock-in, no meaningful coordination benefit to them). From the regulatory seat, Article 17 is a privacy mandate they enforce by monitoring erasure request compliance — their view computes it as rope (coordination between subjects and controllers, no competitive effect in scope). The engine computes per-seat, and these divergences emerge from the structural data; the authored claim (tangled_rope) sits between the snare reading (startup's experience) and the rope reading (regulator's mandate), reflecting the measured mixed structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent_platform_operators sit at d ≈ 0.05-0.15 (beneficiaries: they collect moat value without bearing proportional compliance cost; their infrastructure is legacy-amortized). Startup_data_services sit at d ≈ 0.78-0.85 (targets: they bear infrastructure cost with no equivalent benefit; exit is constrained by market necessity). Emerging_market_entrants sit at d ≈ 0.92-0.98 (near-full targets: they face identical obligations to incumbents but lack any capacity to absorb the cost; they are trapped, not constrained). Data_subject_individuals sit at d ≈ 0.48-0.52 (near-symmetric: they benefit from erasure right in principle but bear identity-lock cost in practice; their formal power is nominally high but institutionally inaccessible). The directionality overrides are not needed for this story — the structural derivation from beneficiary/victim + exit options produces the correct directional map.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (excessive corporate data retention) is contested as status but measurably addressed by market and regulatory pressure outside Article 17. Storage costs, GDPR Article 5 (data minimization), and competitive pressure to build efficient systems have shifted baseline retention practices downward. Article 17 compliance now operates partly as answer to the founding problem (erasure requests still require processing) and partly as infrastructure that persists for reasons independent of the founding problem (competitive lock-in, regulatory path-dependency, vendor interests). The measurement series showing rising extractiveness and theater_ratio even as regulatory compliance matures signals that the constraint's persistence increasingly depends on secondary functions (competitive moat, infrastructure-vendor capture) rather than the founding privacy problem. This is not mandatrophy resolved — the founding problem is not dead, only contested — but it is a live path toward mandatrophy: if entry barriers continue to harden while privacy outcomes plateau, the constraint will have outlived its founding justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_vs_universality,
    'Is Article 17''s universal erasure obligation proportionate to the compliance burden it creates, or does the uniform rule structure impose asymmetric cost that exceeds privacy benefit at smaller scale?',
    'Empirical study comparing erasure request frequency and compliance cost by firm size; regulatory impact assessment comparing privacy outcomes across compliant and proportional-burden regimes (e.g., exemptions for firms under 100 employees, tiered deletion timelines).',
    'If the uniform rule imposes demonstrably disproportionate burden on entrants relative to privacy benefit, reclassify from tangled_rope (mixed coordination + extraction) to snare (extraction justified only by regulatory obligation, not by coordination function). Remedies would shift toward burden-proportionality (carve-outs, scaled compliance, alternative mechanisms).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_vs_universality, empirical, 'Whether the compliance burden is justified by privacy outcomes or is structurally excessive.').

omega_variable(
    infrastructure_necessity_vs_lock_in,
    'Is the specific compliance infrastructure incumbents have built (data-deletion systems, legal frameworks, documentation standards) necessary to achieve erasure compliance, or is it an optimized solution that has become path-dependent lock-in?',
    'Natural experiment: jurisdictions that mandate alternative architectures (e.g., data-deletion via default certification, third-party verification without infrastructure ownership). Comparison of compliance outcomes and cost between architectures.',
    'If alternative architectures achieve equivalent privacy protection at lower cost, the infrastructure lock-in is a contingent choice, not a structural necessity. Reclassify competitive advantage from ''inevitable amortization advantage'' to ''artificial moat.'' Policy interventions could mandate architectural interoperability or cost-sharing models.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(infrastructure_necessity_vs_lock_in, conceptual, 'Whether the incumbent architecture is necessary or contingently locked-in.').

omega_variable(
    founding_problem_resolution_status,
    'Has the underlying privacy problem (excessive data retention) that Article 17 was designed to address already been substantially solved by market forces, regulatory pressure outside Article 17, and storage-cost incentives?',
    'Longitudinal study of corporate data retention practices, comparing pre-GDPR to post-GDPR baseline retention windows, controlling for storage cost and Article 5 (data minimization) pressure. Survey of data subjects on perceived retention practices before and after GDPR.',
    'If retention practices were already shifting downward due to cost and minimization pressure, Article 17 is not solving the founding problem but is maintaining compliance overhead for secondary competitive effects. This supports mandatrophy trajectory: the constraint persists not because the problem exists but because institutional, commercial, and regulatory inertia keep it in place.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_resolution_status, empirical, 'Whether the founding privacy problem is still live or has been substantially addressed by other mechanisms.').

omega_variable(
    identity_lock_mechanism_in_data_subjects,
    'Do data subjects experience suppression of the erasure right as structural (regulatory barriers, technical complexity, institutional friction) or internalized (belief they do not deserve deletion, identity fused with their data record, cognitive patterns learned from platforms)?',
    'Post-exercise suppression trajectory: studies of data subjects after successful erasure request to measure whether they attempt re-engagement with the platform and perceive barriers. Comparison to subjects in jurisdictions with easier erasure mechanisms (no formal request required) to isolate institutional complexity from internalized suppression.',
    'If suppression is primarily structural (technical barriers, administrative friction, platform resistance), entrants could compete by lowering barriers. If suppression is internalized (subjects have learned to accept data retention as inevitable), the barrier is structural but invisible to policy intervention. If mixed, the proportion determines intervention focus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_data_subjects, empirical, 'Whether data-subject suppression is structural or internalized.').

omega_variable(
    alternative_privacy_readings_kernel_contest,
    'Which reading of Article 17 — privacy_fundamental_reading, censorship_mechanism_reading, or competitive_moat_reading — is most defensible within the statute''s text and the founding legislative intent?',
    'Textual analysis of GDPR legislative history, preparatory documents, and committee debates. Comparison to parallel erasure rights in other jurisdictions (California, Brazil) to identify which reading is jurisdiction-independent (fundamental) vs. contingent on EU institutional context (moat-specific). Expert panel on legal interpretation.',
    'If privacy_fundamental is textually and legislatively foundational and competitive_moat is contextual/secondary, this reading is a valid but contingent interpretation. If all three readings are equally supported by text, the kernel is genuinely under-determined and all three constraints are live simultaneously. This affects whether Article 17 should be reformed to narrow the moat effect while preserving privacy, or whether the privacy and moat effects are structurally inseparable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_privacy_readings_kernel_contest, conceptual, 'Which reading of Article 17 the kernel legitimately supports.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__competitive_moat_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article17_erasure_right__competitive_moat_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(arti_tr_t3, article17_erasure_right__competitive_moat_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement(arti_tr_t6, article17_erasure_right__competitive_moat_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(arti_tr_t12, article17_erasure_right__competitive_moat_reading, theater_ratio, 12, 0.46).
narrative_ontology:measurement(arti_tr_t18, article17_erasure_right__competitive_moat_reading, theater_ratio, 18, 0.51).
narrative_ontology:measurement(arti_tr_t25, article17_erasure_right__competitive_moat_reading, theater_ratio, 25, 0.51).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article17_erasure_right__competitive_moat_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(arti_be_t3, article17_erasure_right__competitive_moat_reading, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(arti_be_t6, article17_erasure_right__competitive_moat_reading, base_extractiveness, 6, 0.51).
narrative_ontology:measurement(arti_be_t12, article17_erasure_right__competitive_moat_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(arti_be_t18, article17_erasure_right__competitive_moat_reading, base_extractiveness, 18, 0.66).
narrative_ontology:measurement(arti_be_t25, article17_erasure_right__competitive_moat_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article17_erasure_right__competitive_moat_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(arti_su_t3, article17_erasure_right__competitive_moat_reading, suppression_requirement, 3, 0.32).
narrative_ontology:measurement(arti_su_t6, article17_erasure_right__competitive_moat_reading, suppression_requirement, 6, 0.37).
narrative_ontology:measurement(arti_su_t12, article17_erasure_right__competitive_moat_reading, suppression_requirement, 12, 0.41).
narrative_ontology:measurement(arti_su_t18, article17_erasure_right__competitive_moat_reading, suppression_requirement, 18, 0.42).
narrative_ontology:measurement(arti_su_t25, article17_erasure_right__competitive_moat_reading, suppression_requirement, 25, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__competitive_moat_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article17_erasure_right__competitive_moat_reading, 0.12).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, article17_erasure_right__privacy_fundamental_reading).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, article17_erasure_right__censorship_mechanism_reading).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, gdpr_data_minimization_compliance_burden).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, eu_data_localization_barrier).

% DUAL FORMULATION NOTE:
% Article 17 GDPR is a contested kernel instantiated by three distinct constraint stories: privacy_fundamental_reading (individual erasure as empowerment, negligible extraction), competitive_moat_reading (this story: erasure compliance as barrier to entry, high extraction via infrastructure asymmetry), and censorship_mechanism_reading (erasure weaponization against speech, high extraction via abuse potential). All three are live readings of the same statutory text. No single story captures the constraint; together they form a kernel family. This reading (competitive_moat) influences the other two by establishing that Article 17's enforcement infrastructure creates secondary effects on competitive markets and speech dynamics that interact with the primary privacy and censorship readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
