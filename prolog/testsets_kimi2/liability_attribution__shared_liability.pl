% ============================================================================
% CONSTRAINT STORY: liability_attribution__shared_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__shared_liability, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: liability_attribution__shared_liability
 *   human_readable: Joint AI Liability Distributed by Causal Contribution and Control
 *   domain: technology governance / legal theory / regulatory design
 *
 * SUMMARY:
 *   The EU AI Act and analogous regulatory frameworks are moving toward a
 *   joint liability model for AI systems, distributing accountability between
 *   developers (who create foundational capabilities) and deployers (who
 *   control context of use) according to their respective causal contribution
 *   and control. This constraint story captures the shared_liability reading
 *   of the liability_attribution kernel, which treats neither developers nor
 *   deployers as the sole locus of responsibility. Under this reading, both
 *   parties are drawn into a victim set of compliance costs, insurance
 *   mandates, and contractual opacity mechanisms, while affected individuals
 *   gain a compensation pathway and liability insurers gain a new product
 *   market. The reading coexists with competing developer-primary and
 *   deployer-primary liability readings in different jurisdictions and policy
 *   proposals.
 *
 * KEY AGENTS:
 *   - ai_developers: Primary target (organized / constrained) â bears compliance, insurance, and indemnification costs.
 *   - ai_deployers: Primary target (powerful / constrained) â bears contextual monitoring duties and shared liability exposure.
 *   - affected_individuals: Primary beneficiary (powerless / constrained) â gains expanded damage recovery access.
 *   - liability_insurers: Secondary beneficiary (institutional / arbitrage) â captures premiums from induced demand for coverage.
 *   - regulatory_authorities: Agenda setter (institutional / analytical) â designs and enforces the allocation framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__shared_liability, 0.62).
domain_priors:suppression_score(liability_attribution__shared_liability, 0.55).
domain_priors:theater_ratio(liability_attribution__shared_liability, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, extractiveness, 0.62).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__shared_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__shared_liability, "Joint AI Liability Distributed by Causal Contribution and Control").
narrative_ontology:topic_domain(liability_attribution__shared_liability, "technology governance / legal theory / regulatory design").

domain_priors:requires_active_enforcement(liability_attribution__shared_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__shared_liability, '8ae4c707-43ee-40cf-8d91-73feee977d58').
narrative_ontology:cs_kernel_codification('8ae4c707-43ee-40cf-8d91-73feee977d58', formalized).
narrative_ontology:cs_authority_grounding('8ae4c707-43ee-40cf-8d91-73feee977d58', lineage).
narrative_ontology:cs_interpretation_layer_present('8ae4c707-43ee-40cf-8d91-73feee977d58').
narrative_ontology:cs_reading_relation('8ae4c707-43ee-40cf-8d91-73feee977d58', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('8ae4c707-43ee-40cf-8d91-73feee977d58', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_axiom('8ae4c707-43ee-40cf-8d91-73feee977d58', foundational, liability_tracks_causal_contribution_and_control).
narrative_ontology:cs_axiom_status(liability_tracks_causal_contribution_and_control, holdable).
narrative_ontology:cs_axiom_grounding('8ae4c707-43ee-40cf-8d91-73feee977d58', liability_tracks_causal_contribution_and_control, instrumental).
narrative_ontology:cs_axiom('8ae4c707-43ee-40cf-8d91-73feee977d58', foundational, no_single_primary_locus_of_responsibility).
narrative_ontology:cs_axiom_status(no_single_primary_locus_of_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('8ae4c707-43ee-40cf-8d91-73feee977d58', no_single_primary_locus_of_responsibility, conventional).
narrative_ontology:cs_reference_frame('8ae4c707-43ee-40cf-8d91-73feee977d58', tort_liability_distributed_framework).
narrative_ontology:cs_drift_state('8ae4c707-43ee-40cf-8d91-73feee977d58', ai_act_implementation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8ae4c707-43ee-40cf-8d91-73feee977d58', '').
narrative_ontology:cs_kernel_id(liability_attribution__shared_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, affected_individuals).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, liability_insurers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, ai_developers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, ai_deployers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop foundational AI models and systems. Under shared liability, they must trace model behavior through downstream deployment contexts, purchase specialized liability coverage, and negotiate contractual indemnification clauses with deployers. Exit means abandoning AI development or withdrawing from regulated jurisdictions, both costly.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, ai_developers, payer,
    organized, biographical, constrained, global).

% Integrate and operate AI systems in specific contexts. They bear monitoring and contextual-control duties, must verify developer disclosures, and share liability for harms occurring in deployment. Their exit is constrained because deployment platforms are global and liability follows market access.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, ai_deployers, payer,
    powerful, biographical, constrained, global).

% Individuals harmed by AI system outputs. The shared liability framework expands their ability to recover damages by allowing claims against multiple value-chain actors rather than requiring identification of a single responsible party. They cannot easily exit exposure to AI systems in everyday life.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, affected_individuals, beneficiary,
    powerless, immediate, constrained, national).

% Provide emerging AI liability coverage and indemnification products to developers and deployers. The constraint creates a mandated risk-pool demand. They can enter or exit specific market segments, price risk, and shape contractual terms, capturing premium flows from the extracted compliance burden.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, liability_insurers, beneficiary,
    institutional, generational, arbitrage, global).

% Design and enforce the liability allocation framework, define standards for causal contribution and control, and adjudicate disputes. They set the rules but do not bear the financial costs directly.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__shared_liability, diffuse).
narrative_ontology:fixing_cost_class(liability_attribution__shared_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables compensation for AI harms when traditional single-point liability fails because opaque, multi-actor causal chains prevent clear attribution; distributes incentives for safety investment across developers and deployers.
% TRANSFER_FUNCTION: Moves financial liability, compliance costs, and insurance premiums from a hypothetical single responsible party to multiple nodes along the AI value chain, with allocation keyed to causal contribution and control; transfers premium income to insurers and damage awards to harmed individuals.
% ABSENT_VOICES: Open-source developers lacking corporate legal infrastructure; small deployers without risk-management capacity; future generations subject to long-term systemic AI risks but absent from current contractual allocation; jurisdictions with no AI liability framework whose absence shapes regulatory arbitrage.
% DISAPPEARANCE_RATIONALE: If the shared liability framework vanished, developers and deployers would revert to either immunity or uncertain single-point liability; insurance markets would contract; harmed parties would face higher barriers to recovery; contractual opacity would decrease but so would safety incentives. The AI risk landscape would reorganize around concentrated liability or regulatory vacuum.
% FOUNDING_PROBLEM: AI systems cause foreseeable harm, but conventional product liability and tort law fail because causal chains are technically opaque and span multiple independent actors (developer creates capability, deployer chooses context), leaving victims without recovery and creating under-investment in safety.
% FOUNDING_PROBLEM_CORROBORATION: Consumer protection advocates and tort-law scholars attest the problem from outside the beneficiary set. Tech industry associations and some innovation economists contest the framing, arguing existing negligence and product liability doctrines are sufficient and that distributed liability creates wasteful litigation costs.
narrative_ontology:disappearance_verdict(liability_attribution__shared_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__shared_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__shared_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(liability_attribution__shared_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__shared_liability, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__shared_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__shared_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__shared_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the substantial compliance, insurance, and contractual costs imposed on developers and deployers under a multi-point liability regime. Suppression (0.55) is moderate but rising: the constraint requires active court and regulatory enforcement, and alternatives such as blanket immunity or strict single-point liability are legally suppressed as policy options. Theater ratio (0.40) captures the growing share of activity devoted to contractual opacity and due-diligence theater rather than verifiable risk reduction. Accessibility collapse (0.45) indicates that alternatives (no liability, purely contractual assumption) are partially but not fully closed off; resistance (0.50) reflects active industry pushback against the distributed model. The measurement series share a single time grid to prevent temporal misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The developer seat experiences the constraint as innovation-chilling overreach that forces opaque contractual allocation of unverifiable risks across organizational boundaries. The deployer seat experiences it as a duty to monitor black-box systems they did not create, with liability flowing from contextual decisions they only partially control. The affected-individual seat experiences it as necessary protection. The insurer seat experiences it as a market opportunity. The engine should compute substantial divergence between payer and beneficiary/agenda-setter classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers and deployers are both declared victims with constrained exit, placing their derived directionality near the full-target end; they bear the extracted compliance and insurance costs. Affected individuals occupy a beneficiary seat with constrained exit, receiving the coordination benefit of compensation access but not capturing the financial extraction directly. Liability insurers sit at a beneficiary position with arbitrage-grade exit, allowing them to modulate exposure while capturing premium flows. Regulatory authorities sit at the agenda-setting seat with analytical exit, subsidized by the system's legitimacy needs rather than paying its costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâopaque AI harm with no clear liable partyâremains live and contested, so mandatrophy is not declared. The constraint is not a piton because genuine coordination function (compensation, deterrence) is active and the beneficiary set is nonempty. It is not a snare because the coordination is not mere cover: the harm problem is structurally real. It is a tangled rope because the same framework that coordinates risk distribution also extracts concentrated costs from developers and deployers and generates substantial coordination overhead (contractual opacity, insurance transaction costs).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_contribution_verifiability,
    'Can causal contribution and control actually be verified for advanced AI systems, or does the rule devolve into arbitrary allocation disguised as proportionality?',
    'Empirical study of litigation outcomes and expert-disclosure quality: if damage awards correlate poorly with technical causal contribution metrics, the rule is likely operating as arbitrary allocation.',
    'If contribution is unverifiable, the constraint''s coordination function collapses into a lottery-like extraction mechanism, pushing it toward snare-like operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_contribution_verifiability, empirical, 'Whether the core causal-contribution metric is operationally verifiable.').

omega_variable(
    contractual_opacity_as_extraction,
    'Does the contractual allocation of liability along the value chain serve a genuine risk-management function, or does it function primarily as an opacity mechanism that obscures the true locus and magnitude of extraction?',
    'Comparative analysis of contract terms against realized harm distributions: if indemnification clauses systematically shift liability to the least-informed or least-resourced party regardless of actual control, opacity is extractive.',
    'If opacity is extractive, the constraint''s theater ratio understates the true extraction and the effective directionality for small developers/deployers rises toward full-target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contractual_opacity_as_extraction, conceptual, 'Whether contractual opacity is functional or extractive.').

omega_variable(
    jurisdictional_reading_fragmentation,
    'Does the global coexistence of competing liability readings (developer-primary, deployer-primary, shared) create regulatory arbitrage that undermines the shared_liability constraint''s coordination function?',
    'Tracking jurisdictional migration of AI development and deployment in response to liability regimes: if actors relocate to low-liability jurisdictions, coordination is undermined by fragmentation.',
    'If arbitrage is significant, the constraint''s effective scope is reduced and its classification may shift toward scaffold (if it collapses) or snare (if enforcement becomes coercive against trapped actors in high-liability jurisdictions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_reading_fragmentation, empirical, 'Whether competing kernel readings fragment the constraint''s effectiveness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__shared_liability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_attr_shared_tr_t0, liability_attribution__shared_liability, theater_ratio, 0, 0.15).
narrative_ontology:measurement(liab_attr_shared_tr_t5, liability_attribution__shared_liability, theater_ratio, 5, 0.22).
narrative_ontology:measurement(liab_attr_shared_tr_t10, liability_attribution__shared_liability, theater_ratio, 10, 0.3).
narrative_ontology:measurement(liab_attr_shared_tr_t15, liability_attribution__shared_liability, theater_ratio, 15, 0.36).
narrative_ontology:measurement(liab_attr_shared_tr_t20, liability_attribution__shared_liability, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(liab_attr_shared_be_t0, liability_attribution__shared_liability, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(liab_attr_shared_be_t5, liability_attribution__shared_liability, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(liab_attr_shared_be_t10, liability_attribution__shared_liability, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(liab_attr_shared_be_t15, liability_attribution__shared_liability, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(liab_attr_shared_be_t20, liability_attribution__shared_liability, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(liab_attr_shared_su_t0, liability_attribution__shared_liability, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(liab_attr_shared_su_t5, liability_attribution__shared_liability, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(liab_attr_shared_su_t10, liability_attribution__shared_liability, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(liab_attr_shared_su_t15, liability_attribution__shared_liability, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(liab_attr_shared_su_t20, liability_attribution__shared_liability, suppression_requirement, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__shared_liability, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
