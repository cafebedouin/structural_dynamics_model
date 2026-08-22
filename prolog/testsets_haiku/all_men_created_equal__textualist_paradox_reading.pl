% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__textualist_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__textualist_paradox_reading, []).

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
 *   constraint_id: all_men_created_equal__textualist_paradox_reading
 *   human_readable: Declaration of Independence's Universal Language vs. Restricted Historical Application (Textualist Paradox)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The Declaration of Independence states that 'all men are created equal'
 *   in universal language, yet the founders applied this principle only to
 *   white male property-holders. Originalism (the dominant American
 *   constitutional interpretive framework) claims that this is not a
 *   contradiction: the founders' understanding of 'all men' was legitimately
 *   restricted by their social taxonomy, and that original understanding
 *   governs meaning today. Textualism points out the performative
 *   contradiction: the text uses universal language while the application is
 *   restricted, and no appeal to original intent can make the text's
 *   universality disappear—it remains textually universal while
 *   institutionally applied restrictively. This reading exposes the
 *   originalist framework's logical instability. The constraint is the
 *   enforcement of originalism's claim to reconcile universal language with
 *   restricted application—a claim that requires suppression of the paradox
 *   to maintain institutional credibility.
 *
 * KEY AGENTS:
 *   - Originalist interpretive authority: courts, constitutional scholars, institutional jurisprudence defending the claim that universal language is reconcilable with historical restriction via original understanding
 *   - Excluded populations at founding: structurally absent from founding interpretation, bearing the cost of having their exclusion legitimized by a universal text
 *   - Universalist reform movements: forced to work within originalist framework or challenge institutional authority
 *   - Textualist critics: benefit from exposing the paradox without having to defend an alternative interpretation
 *   - Originalist framework itself: institutional entity whose authority is damaged when the performative contradiction is revealed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, 0.68).
domain_priors:suppression_score(all_men_created_equal__textualist_paradox_reading, 0.71).
domain_priors:theater_ratio(all_men_created_equal__textualist_paradox_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__textualist_paradox_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__textualist_paradox_reading, "Declaration of Independence's Universal Language vs. Restricted Historical Application (Textualist Paradox)").
narrative_ontology:topic_domain(all_men_created_equal__textualist_paradox_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(all_men_created_equal__textualist_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__textualist_paradox_reading, '4021e2d6-04fd-4ce7-b8f1-1a037aa47b21').
narrative_ontology:cs_kernel_codification('4021e2d6-04fd-4ce7-b8f1-1a037aa47b21', fixed_text).
narrative_ontology:cs_authority_grounding('4021e2d6-04fd-4ce7-b8f1-1a037aa47b21', extraction).
narrative_ontology:cs_interpretation_layer_present('4021e2d6-04fd-4ce7-b8f1-1a037aa47b21').
narrative_ontology:cs_reading_relation('4021e2d6-04fd-4ce7-b8f1-1a037aa47b21', all_men_created_equal__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('4021e2d6-04fd-4ce7-b8f1-1a037aa47b21', all_men_created_equal__universalist_reading, influences).
narrative_ontology:cs_axiom('4021e2d6-04fd-4ce7-b8f1-1a037aa47b21', foundational, textual_universality_survives_historical_restriction).
narrative_ontology:cs_axiom_status(textual_universality_survives_historical_restriction, holdable).
narrative_ontology:cs_axiom_grounding('4021e2d6-04fd-4ce7-b8f1-1a037aa47b21', textual_universality_survives_historical_restriction, empirically_contingent).
narrative_ontology:cs_axiom('4021e2d6-04fd-4ce7-b8f1-1a037aa47b21', foundational, originalist_reconciliation_claim_is_incoherent).
narrative_ontology:cs_axiom_status(originalist_reconciliation_claim_is_incoherent, holdable).
narrative_ontology:cs_axiom_grounding('4021e2d6-04fd-4ce7-b8f1-1a037aa47b21', originalist_reconciliation_claim_is_incoherent, deontological).
narrative_ontology:cs_reference_frame('4021e2d6-04fd-4ce7-b8f1-1a037aa47b21', originalist_interpretive_supremacy).
narrative_ontology:cs_drift_state('4021e2d6-04fd-4ce7-b8f1-1a037aa47b21', contemporary_textualist_critique_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('4021e2d6-04fd-4ce7-b8f1-1a037aa47b21', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(all_men_created_equal__textualist_paradox_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, interpretive_authority_defending_originalism).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, excluded_populations_at_founding).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, universalist_constitutional_reform_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, textualist_critics_of_originalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutional apparatus (courts, constitutional scholars, originalist jurisprudence) that interprets the Declaration and Constitution by binding their meaning to 18th-century understanding and usage. This authority must defend the constraint that universal language is reconcilable with restricted application—that 'all men' at founding meant 'all white male property-holders' and that this original understanding governs modern interpretation. The authority enforces this reading through judicial precedent, interpretive doctrine, and credentialed scholarship.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_authority, agenda_setter,
    institutional, generational, constrained, national).

% Enslaved people, women, indigenous peoples, and non-property-holding men who were explicitly or implicitly excluded from the Declaration's promise despite the universal language it deployed. They bear the cost of the constraint because it legitimizes their exclusion as consistent with the founding document's true meaning—the universal language becomes a tool of delegitimization rather than liberation, creating a performative contradiction that must be suppressed to maintain interpretive authority.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, excluded_populations_at_founding, payer,
    powerless, civilizational, trapped, national).

% Abolitionists, suffragists, civil rights advocates, and constitutional reformers who argue that the Declaration's universal language obligates ongoing expansion of equality regardless of original intent. They are forced to operate within a framework where originalist authority claims that universal language is reconcilable with any historical restriction—which means they must either challenge the constraint (expensive, requires cultural/legal majority) or work within it by redefining 'original understanding' iteratively. The constraint suppresses the straightforward reading: the language is actually universal and the application was actually restricted.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, universalist_constitutional_reform_movements, payer,
    organized, generational, constrained, national).

% Constitutional scholars, judges, and legal theorists who benefit from pointing out that the constraint generates a performative contradiction—that the Declaration uses universal language while the originalist reading ties it to restricted historical application. By exposing this paradox, they gain argumentative and credentialing advantage in contemporary jurisprudence. They can claim the originalist framework is incoherent without having to prove an alternative interpretation; the contradiction does the work.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, textualist_critics_of_originalism, beneficiary,
    moderate, biographical, mobile, national).

% The originalist interpretive framework itself, as an institutional and epistemological commitment. It must maintain the claim that universal founding language is compatible with any historical scope restriction by fiat of 'original understanding.' The framework cannot easily exit or revise this constraint without dissolving its core legitimacy claim—that meaning is fixed at founding and not subject to judicial innovation. It is trapped by its own self-definition.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, originalist_legal_tradition, payer,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_non_agent(all_men_created_equal__textualist_paradox_reading, originalist_legal_tradition).

% The broader constitutional conversation including legislators, citizens, political theorists, and institutional actors across traditions. They observe the constraint and its paradox without occupying the agenda-setter role. Some defend originalism, others reject it, others work within it while seeking reform. They carry the cost/benefit of whether the constraint's logical incoherence persists or gets resolved.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, american_constitutional_discourse, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_authority).
narrative_ontology:fixing_cost_class(all_men_created_equal__textualist_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes interpretation of the founding constitutional text by binding its meaning to historical context, preventing judges from reading modern values into the founding language. Coordinates legal interpretation around a fixed anchor (original understanding) rather than allowing each generation to redefine the Constitution's scope.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary moral intuitions and reform movements to the originalist establishment's claim about what the founders meant. Moves credibility and institutional power from universalist readings to originalist readings by rendering the performative contradiction suppressed—the claim that universal language is reconcilable with restricted application is asserted as true despite the logical strain.
% ABSENT_VOICES: The excluded populations at founding are structurally absent from the conversation that established this constraint—they could not participate in declaring what 'all men' meant because they were not counted as men. Contemporary descendants of excluded groups and strict universalist literalists (who would simply read the text as universal and hold it to modern application) remain marginalized by originalist gatekeeping.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared—if originalist authority lost its grip on constitutional interpretation—the field would immediately reorganize around competing readings (living constitutionalism, universalism, progressive originalism) and the performative contradiction would become explicit in jurisprudence. Reform movements would gain credibility, and the Declaration would be read more directly as a universal claim requiring ongoing expansion. The constraint's disappearance would rearrange constitutional authority, but different parties dispute whether this would be rearrangement or revelation of what the text actually says.
% FOUNDING_PROBLEM: The Declaration uses universal language ('all men are created equal') while the founders applied it only to a restricted subset. This created a logical instability in the founding text itself. Originalism 'solves' this by claiming universal language can be reconciled with any historical restriction via original understanding—what the founders meant by 'all men' was actually restricted to their social taxonomy.
% FOUNDING_PROBLEM_CORROBORATION: Textualist and universalist critics of originalism (including Cass Sunstein on constitutional interpretation, Randy Barnett on textualism, and historical scholarship on the Declaration's internal contradictions from non-originalist camps) document that the constraint persists and remains contested. The originalist establishment attests the problem is solved by proper historical analysis; critics outside originalism attest the problem is structural and the constraint masks rather than solves it.
narrative_ontology:disappearance_verdict(all_men_created_equal__textualist_paradox_reading, contested).
narrative_ontology:founding_problem_status(all_men_created_equal__textualist_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__textualist_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(all_men_created_equal__textualist_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__textualist_paradox_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__textualist_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__textualist_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins moderate (0.45) and rises to 0.68 as textualist critique gains visibility and originalism must increasingly suppress the paradox rather than answer it. Theater rises from 0.38 to 0.62, indicating growing performative overhead as originalist scholarship invests in defending the constraint rather than deriving new jurisprudence from it. Suppression requirement rises from 0.55 to 0.71 as the paradox becomes harder to ignore and more institutional force is needed to maintain the claim. The plateau at time 40+ suggests the constraint reaches equilibrium: originalism retains institutional authority despite acknowledged contradiction, performing coherence without achieving it. Accessibility collapse (0.58) is moderate because the paradox is textually obvious—once exposed, the alternative (textualist criticism) is not hard to access. Resistance (0.74) is high because excluded populations, universalist movements, and textualist scholars all have structural reasons to resist originalism's claim. The constraint persists through institutional power, not through logical coherence.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist institutional seat, the constraint solves a real coordination problem: binding interpretation to founding meaning prevents judicial innovation and provides stable ground for constitutional law. From the textualist seat, the constraint is expose-able as incoherent: the text is simply universal and the application is simply restricted; calling this reconcilable by original intent is a logical sleight-of-hand. From the excluded-populations seat, the constraint is extractive: it legitimizes exclusion by making universal language mean restricted application. These gaps should compute to different classifications from the engine: originalists compute something closer to rope (real coordination around interpretive stability); excluded populations and textualists compute snare or tangled_rope (forced participation in a framework that requires suppressing an obvious logical problem).
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist institutional authority is the beneficiary—it collects interpretive supremacy by asserting it can make universal language reconcilable with any historical scope restriction. Its directionality is near 0.0 (beneficiary). Excluded populations and universalist movements are targets—they are forced to work within a framework that suppresses the paradox or fight institutional authority to change it. Their directionality is near 1.0 (full target). Textualist critics sit near 0.5 (symmetric)—they benefit from the paradox by using it as a cudgel against originalism, but they also expose themselves to the same logical problem if they don't offer an alternative. The originalist framework itself is trapped (identity_locked) because its entire institutional identity depends on the coherence claim; it cannot exit without dissolving.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (the Declaration uses universal language while the founders applied it restrictively) is LIVE and CONTESTED. Originalism claims to solve it by appeal to original understanding; textualism and universalism claim it exposes originalism's incoherence rather than resolving it. The disappearance verdict is CONTESTED because if originalist institutional authority weakened, different parties would interpret the constraint's disappearance differently: originalists would say jurisprudence became unstable; universalists would say the text finally meant what it textually says. The mismatch (live founding problem + contested disappearance) indicates a zombie constraint: the problem that created it persists, but the solution (originalism's claim to reconcile universal language with restriction) is logically indefensible. This is the definition of mandatrophy: the constraint persists despite the founding problem remaining unsolved and the solution being acknowledged as incoherent. The high theater ratio (0.62) and rising suppression requirement (0.71) confirm that originalism is maintained through institutional performance rather than logical resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performative_contradiction_suppression_mechanism,
    'Is the measured suppression (0.71) structural (institutional gatekeeping and interpretive authority defending originalism) or internalized (the contradiction genuinely accepted as resolved by proper historical analysis)?',
    'Post-institutional-shift suppression trajectory: if the performative contradiction persists or intensifies after originalist institutional dominance weakens, reclassify as partially or fully internalized. Track whether alternative interpretive frameworks still exhibit suppression of the paradox even when institutional pressure to defend originalism declines.',
    'If internalized, the constraint''s effective suppression is higher than the institutional measure suggests; the paradox is carried forward not by institutional force but by internalized acceptance of the originalist framework. This would deepen the classification toward snare rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performative_contradiction_suppression_mechanism, empirical, 'Structural vs. internalized suppression of the performative contradiction in originalist framework.').

omega_variable(
    kernel_stability_under_textualist_exposure,
    'Can the originalist reading maintain its authority claim after the performative contradiction is explicitly exposed and widely acknowledged?',
    'Track whether originalist scholarship responds to textualist critique by (a) conceding the contradiction but arguing it is historically accurate anyway, (b) reformulating the original understanding to absorb the criticism, or (c) doubling down on institutional authority despite the logical strain. The response pattern will show whether the kernel is stable or degrading.',
    'If originalism maintains institutional authority despite acknowledged contradiction, the constraint persists but moves closer to piton (performance defended by inertia rather than coherence). If the contradiction forces reformulation, the reading itself may shift. If authority erodes, the constraint may convert to snare (pure extraction with no coordination claim) or dissolve.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_stability_under_textualist_exposure, conceptual, 'Whether originalist framework can survive explicit textualist paradox exposure.').

omega_variable(
    universalist_reading_kernel_contest,
    'Is the universalist reading of the Declaration a separate constraint or a claim about THIS constraint''s instability?',
    'Treat universalism as a separate, competing constraint (its own story with its own ε, stakeholders, and claimed type) that SHARES the kernel with originalism and textualism. The three readings are three distinct stories, each instantiating a different constraint from the same text. This textualist reading EXPOSES the originalist constraint''s paradox; it does not resolve it or claim to be the ''true'' reading.',
    'Clarifies the kernel structure: three readings, three constraints, one contested text. The textualist reading''s function is diagnostic (exposing the originalist paradox) rather than prescriptive (offering a resolution). Its extractiveness (0.68) reflects that it gains argumentative authority by pointing out the contradiction, not by offering a working alternative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universalist_reading_kernel_contest, conceptual, 'Textualism as paradox-exposure distinct from universalism as alternative reading.').

omega_variable(
    victim_identity_originalist_framework_itself,
    'Is the listed victim ''originalist_interpretive_framework'' a real agent or an abstraction?',
    'The framework is not a person, but it is an institutional entity (courts, scholarship, jurisprudence) whose legitimacy depends on the constraint. When the constraint''s logical incoherence is exposed, the framework suffers reputational and institutional damage. Track whether institutional originalism experiences reduced credibility, recruitment, or funding after textualist critique gains visibility.',
    'The framework''s agency is institutional; exposing the paradox delegitimizes it by showing its core claim (universal language is reconcilable with any historical restriction) is logically indefensible. This is extraction in the sense that originalism collects institutional authority by suppressing the paradox; when suppression fails, the authority is reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_identity_originalist_framework_itself, empirical, 'Whether originalist institutional framework suffers measurable delegitimation from textualist paradox exposure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__textualist_paradox_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t0, all_men_created_equal__textualist_paradox_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(all__tr_t8, all_men_created_equal__textualist_paradox_reading, theater_ratio, 8, 0.43).
narrative_ontology:measurement(all__tr_t16, all_men_created_equal__textualist_paradox_reading, theater_ratio, 16, 0.49).
narrative_ontology:measurement(all__tr_t24, all_men_created_equal__textualist_paradox_reading, theater_ratio, 24, 0.55).
narrative_ontology:measurement(all__tr_t32, all_men_created_equal__textualist_paradox_reading, theater_ratio, 32, 0.6).
narrative_ontology:measurement(all__tr_t40, all_men_created_equal__textualist_paradox_reading, theater_ratio, 40, 0.62).
narrative_ontology:measurement(all__tr_t50, all_men_created_equal__textualist_paradox_reading, theater_ratio, 50, 0.62).

% Extraction over time
narrative_ontology:measurement(all__be_t0, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(all__be_t8, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(all__be_t16, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(all__be_t24, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(all__be_t32, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(all__be_t40, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(all__be_t50, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t0, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(all__su_t8, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(all__su_t16, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(all__su_t24, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(all__su_t32, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(all__su_t40, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(all__su_t50, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__textualist_paradox_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(all_men_created_equal__textualist_paradox_reading, 0.14).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__universalist_reading).

% DUAL FORMULATION NOTE:
% The kernel 'all_men_created_equal' decomposes into three distinct constraints: (1) originalist_reading (claims universal language is reconcilable with historical restriction via original understanding), (2) textualist_paradox_reading (this constraint: exposes the logical incoherence of reconciling universal text with restricted application), (3) universalist_reading (claims universal language obligates ongoing expansion of equality). Each reading instantiates a different constraint with its own ε, stakeholders, and classification. The textualist reading's function is diagnostic—it exposes the originalist constraint's instability by pointing out the performative contradiction—rather than prescriptive like universalism. All three share the kernel text; they differ in how they read it and what they claim about its coherence. The textualist reading influences both sibling readings: it creates structural pressure on originalism by making the contradiction unavoidable, and it provides argumentative foundation for universalism's claim that a straight reading of universal language is more defensible than originalism's historical restriction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(all_men_created_equal__textualist_paradox_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
