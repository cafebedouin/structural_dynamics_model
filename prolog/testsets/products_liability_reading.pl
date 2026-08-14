% ============================================================================
% CONSTRAINT STORY: products_liability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_products_liability_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: products_liability_reading
 *   human_readable: Products-Liability Reading of Algorithmic Attribution (Escola's Grammar Applied to Software)
 *   domain: law_and_technology/products_liability/first_amendment_doctrine
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested kernel 'algorithmic
 *   attribution' — the question of what an algorithmic
 *   curation/recommendation system's output should be attributed TO for legal
 *   purposes, and to whom liability for its effects should attach. Under the
 *   products-liability reading, the firm operating the algorithm is treated
 *   the way Escola v. Coca-Cola Bottling Co. treated the bottler: the
 *   plaintiff cannot inspect the internal process that produced the harmful
 *   output, the firm controls that process end to end, and the firm can
 *   spread or reduce the risk through design change, insurance, or pricing.
 *   Attribution to the firm therefore establishes loss-allocation
 *   responsibility independent of proof of firm intent or specific fault —
 *   the algorithm is a product feature subject to design-defect analysis, not
 *   protected editorial speech. This is a live, contested doctrinal position,
 *   not settled law; sibling readings characterize the same underlying
 *   algorithmic-attribution question as an expressive-speech question, a
 *   conduct-regulation question, or a question of technician intent, and each
 *   produces a structurally different set of victims, beneficiaries, and
 *   liability exposure. This story authors ONLY the products-liability
 *   reading as its own constraint with its own stable epsilon.
 *
 * KEY AGENTS:
 *   - injured_users: powerless/trapped — bear algorithmic harm, cannot inspect the system, benefit from the enterprise-liability route this reading opens
 *   - platform_operators: institutional/constrained — control the design process, can spread risk, bear the liability exposure this reading imposes
 *   - algorithm_developers: organized/constrained — make the design decisions that become the evidentiary target, without personally bearing liability
 *   - plaintiffs_bar: organized/mobile — beneficiary of the doctrinal opening; builds litigation practice on design-defect theories
 *   - downstream_smaller_platforms: moderate/constrained — bear disproportionate compliance and litigation-defense cost relative to their scale
 *   - courts_applying_doctrine: institutional/analytical — set the doctrine's actual boundaries case by case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(products_liability_reading, 0.52).
domain_priors:suppression_score(products_liability_reading, 0.38).
domain_priors:theater_ratio(products_liability_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(products_liability_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(products_liability_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(products_liability_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(products_liability_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(products_liability_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(products_liability_reading, tangled_rope).
narrative_ontology:human_readable(products_liability_reading, "Products-Liability Reading of Algorithmic Attribution (Escola's Grammar Applied to Software)").
narrative_ontology:topic_domain(products_liability_reading, "law_and_technology/products_liability/first_amendment_doctrine").

domain_priors:requires_active_enforcement(products_liability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(products_liability_reading, '5a9a7cdb-dc5a-43be-9974-8101c9749d7b').
narrative_ontology:cs_kernel_codification('5a9a7cdb-dc5a-43be-9974-8101c9749d7b', distributed).
narrative_ontology:cs_authority_grounding('5a9a7cdb-dc5a-43be-9974-8101c9749d7b', distributed).
narrative_ontology:cs_reading_relation('5a9a7cdb-dc5a-43be-9974-8101c9749d7b', products_liability_reading__expressive_attribution_reading, forecloses).
narrative_ontology:cs_reading_relation('5a9a7cdb-dc5a-43be-9974-8101c9749d7b', products_liability_reading__conduct_regulation_reading, coexists_with).
narrative_ontology:cs_reading_relation('5a9a7cdb-dc5a-43be-9974-8101c9749d7b', products_liability_reading__technician_intent_reading, influences).
narrative_ontology:cs_axiom('5a9a7cdb-dc5a-43be-9974-8101c9749d7b', foundational, control_of_process_grounds_liability_regardless_of_fault).
narrative_ontology:cs_axiom_status(control_of_process_grounds_liability_regardless_of_fault, holdable).
narrative_ontology:cs_axiom_grounding('5a9a7cdb-dc5a-43be-9974-8101c9749d7b', control_of_process_grounds_liability_regardless_of_fault, instrumental).
narrative_ontology:cs_axiom('5a9a7cdb-dc5a-43be-9974-8101c9749d7b', foundational, information_asymmetry_between_firm_and_plaintiff_justifies_burden_shift).
narrative_ontology:cs_axiom_status(information_asymmetry_between_firm_and_plaintiff_justifies_burden_shift, holdable).
narrative_ontology:cs_axiom_grounding('5a9a7cdb-dc5a-43be-9974-8101c9749d7b', information_asymmetry_between_firm_and_plaintiff_justifies_burden_shift, empirically_contingent).
narrative_ontology:cs_reference_frame('5a9a7cdb-dc5a-43be-9974-8101c9749d7b', escola_enterprise_liability_baseline).
narrative_ontology:cs_drift_state('5a9a7cdb-dc5a-43be-9974-8101c9749d7b', contemporary_algorithmic_curation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5a9a7cdb-dc5a-43be-9974-8101c9749d7b', '').
narrative_ontology:cs_kernel_id(products_liability_reading, algorithmic_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(products_liability_reading, injured_users).
narrative_ontology:constraint_beneficiary(products_liability_reading, plaintiffs_bar).
narrative_ontology:constraint_beneficiary(products_liability_reading, safety_conscious_competitors).
narrative_ontology:constraint_victim(products_liability_reading, platform_operators).
narrative_ontology:constraint_victim(products_liability_reading, algorithm_developers).
narrative_ontology:constraint_victim(products_liability_reading, downstream_smaller_platforms).
narrative_ontology:constraint_vindicates(products_liability_reading, enterprise_liability_doctrine).
narrative_ontology:constraint_vindicates(products_liability_reading, risk_spreading_rationale).
narrative_ontology:constraint_vindicates(products_liability_reading, information_asymmetry_justifies_strict_liability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals harmed by algorithmic curation outcomes (radicalization pathways, product-defect-style recommendation harms, discriminatory ranking effects). They cannot inspect the ranking system's internals, cannot reconstruct why a particular output reached them, and bear the injury regardless of whether they can prove causation under ordinary negligence pleading. Attribution to the firm as producer gives them a route to recovery that does not require proving the firm's subjective intent or specific knowledge.
narrative_ontology:constraint_stakeholder(products_liability_reading, injured_users, beneficiary,
    powerless, biographical, trapped, national).

% Firms that design, deploy, and continuously modify recommendation and ranking systems. Under this reading they answer for systemic effects because they control the design process end to end, can run internal testing the plaintiff cannot replicate, and can price the liability risk into insurance, reserves, or design changes across their entire user base. They resist the reading because it exposes them to liability exposure structurally similar to a manufacturer of a defective consumer product, rather than the narrower fault-based standard they argue should apply to expressive or editorial choices.
narrative_ontology:constraint_stakeholder(products_liability_reading, platform_operators, payer,
    institutional, generational, constrained, national).

% Engineering teams that build and iterate the ranking systems inside firms. They set day-to-day design choices (the actual locus of the alleged 'defect') but do not personally bear liability under enterprise theory; the firm answers in their place. Their design decisions become the evidentiary substrate for design-defect claims, and internal engineering documents become discoverable in litigation.
narrative_ontology:constraint_stakeholder(products_liability_reading, algorithm_developers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(products_liability_reading, algorithm_developers, agenda_setter).

% Plaintiff-side litigators who bring design-defect and failure-to-warn theories against platforms. They benefit directly from a doctrinal frame that treats the algorithm as a product feature rather than protected speech, since it opens discovery and a viable cause of action that would otherwise be foreclosed by an expressive-conduct characterization.
narrative_ontology:constraint_stakeholder(products_liability_reading, plaintiffs_bar, beneficiary,
    organized, biographical, mobile, national).

% Firms that have already invested in safer design practices or more conservative ranking systems. A products-liability regime that rewards demonstrable safety engineering can advantage them relative to competitors who externalize design risk, though this benefit is incidental to the doctrine's core operation.
narrative_ontology:constraint_stakeholder(products_liability_reading, safety_conscious_competitors, beneficiary,
    powerful, biographical, mobile, national).

% Smaller firms and startups running recommendation or ranking systems who lack the litigation budgets, insurance capacity, and internal testing infrastructure of dominant platforms. The same doctrinal exposure that a large platform can absorb through risk-spreading falls on them as a disproportionate compliance and litigation-defense burden, potentially entrenching incumbents who can afford the liability overhead.
narrative_ontology:constraint_stakeholder(products_liability_reading, downstream_smaller_platforms, payer,
    moderate, biographical, constrained, national).

% Judges and juries who decide, case by case, whether to characterize an algorithm's output as a product defect subject to Escola-style strict/enterprise liability or as editorial judgment protected from that framing. Their doctrinal choices set the reading's boundaries and determine which cases proceed to discovery.
narrative_ontology:constraint_stakeholder(products_liability_reading, courts_applying_doctrine, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates the cost of investigating and remedying systemic algorithmic harms to the party with the technical capacity, cost information, and risk-spreading mechanisms to address them, instead of leaving the cost to fall on whichever individual user happens to be harmed and cannot access the system's internals.
% TRANSFER_FUNCTION: Moves the burden of proving what went wrong, and the cost of harm when something did, from injured individual users (who cannot inspect the process) to the firm that designed and operates the system (who can inspect it, and can spread the cost across its user base or through insurance).
% ABSENT_VOICES: The algorithm developers whose specific engineering choices become the evidentiary target rarely appear as named parties even though their decisions are litigated; their voice is filtered entirely through corporate counsel and discovery responses. Users who are structurally similar to plaintiffs but never litigate (because they cannot identify the harm's algorithmic source) never enter the record at all — the doctrine's beneficiary class is narrower than its intended reach.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and algorithmic curation reverted fully to the sibling expressive-attribution or technician-intent framings, injured users would lose the enterprise-liability route entirely: they would need to prove either firm intent/knowledge (a far higher bar) or that the output was unprotected conduct rather than protected editorial expression. Discovery into design choices would close, plaintiffs'-bar litigation practice built on design-defect theories would collapse, and platforms would face materially lower liability exposure. The rearrangement runs through active dockets, not merely theoretical doctrine.
% FOUNDING_PROBLEM: Ordinary negligence and contract doctrine assumed a plaintiff could investigate what a defendant did and show fault; mass-produced, opaque, continuously-updated software systems break that assumption because no user can inspect an algorithm's internal decision process, while the firm that built it can. Escola's original solution to an analogous problem in physical manufacturing (the injured consumer cannot show how the bottler's process failed, but the bottler can) is imported to solve the same information asymmetry in software.
% FOUNDING_PROBLEM_CORROBORATION: Products-liability scholars and consumer-protection litigators outside the plaintiffs' bar corroborate that the information-asymmetry problem is real and unresolved by existing negligence doctrine. Platform-side counsel and First Amendment scholars dispute that the problem calls for THIS doctrinal solution, arguing the analogy to manufactured goods breaks down where the system's output is itself expressive content rather than a physical defect; that dispute is the live doctrinal fight this reading is one side of, not a settled genealogy.
narrative_ontology:disappearance_verdict(products_liability_reading, world_rearranges).
narrative_ontology:founding_problem_status(products_liability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(products_liability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-13',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(products_liability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(products_liability_reading, 0.52, 'claude-sonnet-5', 'algorithmic_authorless_harm_2026_20260813_215102', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(products_liability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(products_liability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(products_liability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is authored as moderate: enterprise liability is a real coordination solution to a real information-asymmetry problem, not pure extraction, but it does transfer real cost from firms to a party (algorithm developers, indirectly, and smaller downstream platforms) that did not choose the enterprise-liability frame and cannot always spread the cost the way dominant platforms can. Suppression (0.38) reflects active enforcement machinery — discovery obligations, litigation exposure, potential regulatory codification — but this is lower than a pure extraction constraint because the doctrine remains genuinely contested in courts rather than settled and coercively imposed. Theater ratio (0.28) is modest: most of the activity under this reading is substantive litigation and design change, not performative compliance, though some firms adopt visible 'safety by design' documentation partly for litigation-defense theater rather than actual harm reduction. Accessibility collapse (0.42) is mid-range: firms retain real alternative doctrinal arguments (First Amendment defenses, Section 230-adjacent arguments) that have not collapsed, so this is not a closed field. Resistance (0.58) is substantial and reflects the genuinely live doctrinal fight — platforms litigate hard against this characterization precisely because the sibling expressive-attribution reading would foreclose most of this liability exposure.
 *
 * PERSPECTIVAL GAP:
 *   From the injured-user seat, attribution to the firm looks like ordinary enterprise-liability coordination: the party best positioned to know and to fix the problem should answer for it, exactly as Escola held for the bottler. From the platform-operator seat, the same attribution looks like extraction dressed in products-liability language — a doctrinal import that treats expressive, constantly-updated, difficult-to-characterize-as-'defective' software as if it were a physical consumer good, exposing the firm to liability for editorial judgments that would be protected speech under a different characterization. The engine should compute these as structurally different seat experiences of the same authored metrics, not as a contradiction to be resolved.
 *
 * DIRECTIONALITY LOGIC:
 *   Injured users are declared beneficiaries of this reading because it is the doctrinal vehicle that gives them a viable claim at all; their exit option is trapped (they cannot avoid algorithmic curation systems as a condition of participating in modern digital life) which pushes their derived directionality toward the target/high-d end on the underlying HARM, but toward the beneficiary end on THIS constraint (the liability doctrine), since the doctrine is what redistributes cost toward the firm on their behalf. Platform operators and algorithm developers are declared victims of the liability exposure this reading creates — constrained exit because they cannot simply exit the market or abandon algorithmic curation without abandoning their business model, and cannot easily relitigate the underlying kernel question in every case. Downstream smaller platforms carry the heaviest relative burden despite comparable formal exit options to large platforms, because their actual capacity to spread risk through insurance or litigation budgets is far lower — this is a same-nominal-power, different-real-exit divergence worth flagging structurally even though both occupy 'moderate' to 'institutional' power bands.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — plaintiffs cannot inspect an opaque, firm-controlled process that caused them harm — remains genuinely live wherever algorithmic systems are proprietary and non-auditable, so this is not a case of an arrangement persisting after its function died. What prevents mislabeling this as pure extraction is the coordination function still operating: courts applying the doctrine continue to require a showing of design defect or causal link to the algorithm's specific behavior, not mere firm involvement, which distinguishes it from a snare that would extract from firms regardless of actual fault or defect. What prevents mislabeling this as pure coordination is the genuine asymmetric cost imposed on downstream smaller platforms and on algorithm developers whose design choices become litigation targets without their personal say in whether the doctrine applies — that asymmetry, plus the requirement of active enforcement (litigation, discovery, potential regulatory codification) to keep the doctrine operative against resistant platforms, is why this authors as tangled_rope rather than rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithm_as_product_vs_speech_boundary,
    'Is an algorithmic ranking/recommendation output structurally more like a manufactured product defect (Escola''s bottle) or more like an editorial judgment (a newspaper''s front-page placement)? The products-liability reading depends on the former characterization holding as a matter of doctrine, but this is precisely the contested question the sibling readings answer differently.',
    'Appellate resolution of the design-defect-vs-speech characterization question, ideally at a supreme court level, applied specifically to algorithmic curation systems rather than analogized from either physical products or traditional editorial media.',
    'If courts settle on the products characterization, this reading''s liability exposure becomes durable doctrine rather than a contested position; if courts settle on the speech characterization, this reading is foreclosed and the expressive_attribution_reading becomes the governing frame, with a correspondingly different victim/beneficiary structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(algorithm_as_product_vs_speech_boundary, conceptual, 'Whether algorithmic outputs are doctrinally products or speech is unresolved and determines which kernel reading governs.').

omega_variable(
    risk_spreading_capacity_asymmetry,
    'Does the risk-spreading rationale that justifies enterprise liability for large platforms actually hold for downstream smaller platforms, or does it impose a cost they cannot in fact spread, converting coordination into disproportionate extraction at the smaller end of the market?',
    'Empirical study of insurance availability, litigation cost burden, and market exit/consolidation patterns among smaller algorithmic-curation firms following adoption of this doctrinal reading in a given jurisdiction.',
    'If smaller platforms cannot spread risk, the doctrine''s coordination justification weakens specifically for that stakeholder class, supporting a scaled or tiered liability standard rather than uniform enterprise liability; if they can spread risk adequately, the current uniform application is better justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_spreading_capacity_asymmetry, empirical, 'Whether the risk-spreading premise holds uniformly across firm size, or breaks down for smaller platforms.').

omega_variable(
    sibling_reading_selection_pressure,
    'Which of the four kernel readings a given court or jurisdiction adopts may depend less on doctrinal reasoning and more on which litigants have resources to shape precedent first (well-funded plaintiffs'' bar pushing products-liability framing vs. well-funded platforms pushing speech framing) — is the eventual dominant reading determined by legal merit or by asymmetric litigation capacity?',
    'Comparative analysis of case outcomes correlated with litigant resources across jurisdictions, controlling for underlying facts, to see whether reading-adoption tracks doctrinal reasoning or party resources.',
    'If litigation capacity drives reading selection more than doctrinal merit, the kernel''s eventual settled reading may reflect resource asymmetry rather than the better argument, which matters for how much normative weight to place on whichever reading eventually ''wins.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_selection_pressure, empirical, 'Whether kernel-reading dominance tracks legal merit or asymmetric litigation resources.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(products_liability_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prod_tr_t0, products_liability_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(prod_tr_t4, products_liability_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(prod_tr_t8, products_liability_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(prod_tr_t12, products_liability_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(prod_tr_t16, products_liability_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(prod_tr_t20, products_liability_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(prod_be_t0, products_liability_reading, base_extractiveness, 0, 0.31).
narrative_ontology:measurement(prod_be_t4, products_liability_reading, base_extractiveness, 4, 0.36).
narrative_ontology:measurement(prod_be_t8, products_liability_reading, base_extractiveness, 8, 0.41).
narrative_ontology:measurement(prod_be_t12, products_liability_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(prod_be_t16, products_liability_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(prod_be_t20, products_liability_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(prod_su_t0, products_liability_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(prod_su_t4, products_liability_reading, suppression_requirement, 4, 0.26).
narrative_ontology:measurement(prod_su_t8, products_liability_reading, suppression_requirement, 8, 0.29).
narrative_ontology:measurement(prod_su_t12, products_liability_reading, suppression_requirement, 12, 0.32).
narrative_ontology:measurement(prod_su_t16, products_liability_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(prod_su_t20, products_liability_reading, suppression_requirement, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(products_liability_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(products_liability_reading, 0.12).
narrative_ontology:affects_constraint(products_liability_reading, expressive_attribution_reading).
narrative_ontology:affects_constraint(products_liability_reading, conduct_regulation_reading).
narrative_ontology:affects_constraint(products_liability_reading, technician_intent_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the algorithmic_attribution kernel, each a separate constraint with its own epsilon, beneficiary/victim structure, and classification. products_liability_reading treats the firm as answerable through enterprise-liability/design-defect doctrine (epsilon 0.52, tangled_rope). expressive_attribution_reading would treat the same algorithmic output as protected firm speech (expected low epsilon for the firm, near-mountain-like First Amendment protection, and a correspondingly different victim set that excludes injured users from a liability remedy). conduct_regulation_reading would treat the output as regulable conduct outside both products and speech frames. technician_intent_reading would require a fault showing tied to specific engineers' knowledge or intent, closer to ordinary negligence. These are NOT the same constraint measured four ways — they are four constraints with genuinely different epsilon values, different stakeholders, and different doctrinal consequences, linked here because they share the same underlying kernel (what does an algorithm's output get attributed TO, and by whom is that attribution contested).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
