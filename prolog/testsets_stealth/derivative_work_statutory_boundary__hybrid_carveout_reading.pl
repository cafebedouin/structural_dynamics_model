% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__hybrid_carveout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__hybrid_carveout_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__hybrid_carveout_reading
 *   human_readable: Derivative Work Boundary — Commercial Carveout Reading
 *   domain: legal/economic/technological
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the
 *   derivative_work_statutory_boundary kernel: the hybrid carveout reading,
 *   under which the lawful/unlawful line for transformative reuse of
 *   copyrighted expression runs along commercial status — non-commercial
 *   transformation is permitted, commercial exploitation requires
 *   authorization. Under the epsilon-invariance principle this is a separate
 *   constraint from its siblings, not a variant view of one thing: the
 *   enclosure_reading (any incorporative use is a derivative preparation)
 *   authors a high-epsilon constraint whose victim set includes
 *   non-commercial creators; the coordination_reading (only fixed,
 *   substantially incorporating recastings are restricted) authors a
 *   low-epsilon constraint with almost no extraction from transformative
 *   practice. This reading sits between them with a categorical beneficiary
 *   split: rights holders and licensing intermediaries collect from the
 *   commercial side, non-commercial creators are subsidized by the carveout,
 *   and commercial developers bear licensing costs that scale with
 *   commerciality rather than with demonstrated harm. The claim/metric gap is
 *   deliberate: the reading is CLAIMED as tangled_rope because it plausibly
 *   carries both a genuine coordination function (pricing expressive reuse
 *   while preserving an open non-commercial commons) and asymmetric
 *   extraction (fees keyed to a proxy — commerciality — rather than to
 *   substitution), and the authored metrics describe that mixed operation
 *   independently; the engine measures the divergence per seat.
 *
 * KEY AGENTS:
 *   - copyright_holders: Primary beneficiary and enforcement agenda-setter (institutional/arbitrage) — own the catalogs, choose whom to license and whom to sue, collect the authorization revenues
 *   - commercial_developers: Primary target (powerful/constrained) — bear the licensing burden whenever transformative reuse touches commerce
 *   - noncommercial_creators: Carveout beneficiaries (moderate/constrained) — operate inside the permitted zone, bounded by the commercial line they must not cross
 *   - open_source_communities: Split-position actors (moderate/constrained) — mostly subsidized by the carveout, exposed when projects commercialize
 *   - licensing_intermediaries: Secondary beneficiary (organized/mobile) — run the clearance and royalty machinery the authorization requirement feeds
 *   - federal_courts: Administering authority (institutional/analytical) — draw the operative line case by case
 *   - ai_training_developers: Excluded challengers (powerful/arbitrage) — contest the boundary's reach from outside the framework
 *   - downstream_audiences: Diffuse cost-bearers (powerless/mobile) — absorb pass-through costs and lost transformative works
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.62).
domain_priors:suppression_score(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.7).
domain_priors:theater_ratio(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__hybrid_carveout_reading, tangled_rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__hybrid_carveout_reading, "Derivative Work Boundary — Commercial Carveout Reading").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__hybrid_carveout_reading, "legal/economic/technological").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__hybrid_carveout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__hybrid_carveout_reading, '712c2cce-f7ab-47fc-aa7a-319b05dc76cf').
narrative_ontology:cs_kernel_codification('712c2cce-f7ab-47fc-aa7a-319b05dc76cf', formalized).
narrative_ontology:cs_authority_grounding('712c2cce-f7ab-47fc-aa7a-319b05dc76cf', lineage).
narrative_ontology:cs_interpretation_layer_present('712c2cce-f7ab-47fc-aa7a-319b05dc76cf').
narrative_ontology:cs_reading_relation('712c2cce-f7ab-47fc-aa7a-319b05dc76cf', derivative_work_statutory_boundary__enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('712c2cce-f7ab-47fc-aa7a-319b05dc76cf', derivative_work_statutory_boundary__coordination_reading, coexists_with).
narrative_ontology:cs_axiom('712c2cce-f7ab-47fc-aa7a-319b05dc76cf', foundational, commercial_status_conditions_infringement).
narrative_ontology:cs_axiom_status(commercial_status_conditions_infringement, holdable).
narrative_ontology:cs_axiom_grounding('712c2cce-f7ab-47fc-aa7a-319b05dc76cf', commercial_status_conditions_infringement, instrumental).
narrative_ontology:cs_axiom('712c2cce-f7ab-47fc-aa7a-319b05dc76cf', foundational, noncommercial_transformation_presumptively_free).
narrative_ontology:cs_axiom_status(noncommercial_transformation_presumptively_free, holdable).
narrative_ontology:cs_axiom_grounding('712c2cce-f7ab-47fc-aa7a-319b05dc76cf', noncommercial_transformation_presumptively_free, deontological).
narrative_ontology:cs_reference_frame('712c2cce-f7ab-47fc-aa7a-319b05dc76cf', adaptation_market_protection_frame).
narrative_ontology:cs_drift_state('712c2cce-f7ab-47fc-aa7a-319b05dc76cf', contemporary_platform_economy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('712c2cce-f7ab-47fc-aa7a-319b05dc76cf', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, licensing_intermediaries).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, noncommercial_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, open_source_communities).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, downstream_audiences).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, open_source_communities).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, downstream_audiences).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own catalogs of protected expression spanning text, music, film, and software. Decide which commercial reuses receive licenses, at what rates, and which proceed to litigation instead. Collect authorization revenues directly and through intermediaries, and selectively tolerate some unlicensed uses while prosecuting others. Because they hold the assets, their exit takes the form of choosing enforcement venues and targets rather than leaving the arrangement.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders, beneficiary).

% Build products that incorporate, adapt, sample, or extend existing protected expression and intend to sell the result. Face a clearance step whose cost scales with their commerciality rather than with any measured effect on the rights holder's markets. Options are negotiating licenses, redesigning around the borrowed expression, or litigating; abandoning commerce converts them into the permitted class but forfeits the revenue that justified the project. Large incumbents often hold licensing portfolios on both sides of the table.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_developers, payer,
    powerful, biographical, constrained, global).

% Make transformative works — fan fiction, remixes, mods, commentary, covers — without charging for them, and operate inside the zone the carveout leaves open. Their freedom is bounded by the commercial line: accepting tips, running ads, or selling prints can flip their status and expose them to takedown or suit. Many police themselves short of monetization they could technically pursue.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, noncommercial_creators, beneficiary,
    moderate, biographical, constrained, global).

% Distributed developer communities whose non-commercial building on existing expression is sheltered by the carveout, and whose norms assume that openness. When a community project accepts sponsorship, sells support, or is acquired, its past reuse can retroactively acquire a price tag, and the community faces a choice between relicensing, reverting contributions, or disputing the characterization. Most members experience the permission; the minority who commercialize experience the bill.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, open_source_communities, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, open_source_communities, payer).

% Collective rights organizations, clearance houses, and royalty administrators that operate the machinery connecting commercial reusers with rights holders. Their revenue is a function of authorization volume and rate; they maintain databases, audit usage, and distribute proceeds minus administrative cuts. If the authorization requirement narrowed, their business would shrink to the remaining licensed categories, and several have diversified into adjacent rights administration.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, licensing_intermediaries, beneficiary,
    organized, biographical, mobile, global).

% Adjudicate where the commercial line falls case by case, weighing transformativeness, market effect, and the character of the use. Their opinions are the operative text of the boundary between statutory revisions, and each ruling reallocates freedom between the permitted and licensed zones. They take the positions of the other seats as adversarial input rather than experiencing the arrangement's costs or gains directly.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, federal_courts, observer).

% Build systems trained on large corpora that include protected expression, arguing that training is transformative intermediate use rather than preparation of a derivative work. They hold no seat in the statutory framework the boundary administers and are shaping the doctrine through litigation fought from outside it. Their alternatives include jurisdictional relocation, licensed-data deals, and synthetic corpora, each carrying cost or capability tradeoffs.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, ai_training_developers, excluded,
    powerful, biographical, arbitrage, global).

% Consume adaptations, remixes, sequels, and commentary built on existing expression. They receive authorized works whose licensing costs are passed through in prices, and they lose unauthorized transformative works that never clear the line and are withdrawn or never published. They are represented in the arrangement only indirectly, through market behavior and amicus positions, and can shift attention to unaffected content.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, downstream_audiences, payer,
    powerless, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, downstream_audiences, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__hybrid_carveout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels commercial reuse of protected expression through a negotiable authorization step — solving the pricing problem for expressive adaptation that bilateral bargaining could not solve at scale — while leaving a free non-commercial zone that preserves follow-on creativity without requiring every transformer to clear rights first.
% TRANSFER_FUNCTION: Moves licensing revenue from commercial reusers of protected expression to rights holders (with administrative cuts to intermediaries), and moves legal security to non-commercial transformers in the form of a permission they did not previously hold.
% ABSENT_VOICES: Downstream audiences have no seat in clearance negotiations and feel the outcome only as prices and vanished works. Machine-learning developers contest the boundary's reach from outside the framework. The public-domain interest — the claim that broader freedom to build on expression serves future creation — is argued by scholars and amici but holds no standing seat; dissent enters the process mainly after suits are filed, not before terms are set.
% DISAPPEARANCE_RATIONALE: If the commerciality-keyed boundary vanished overnight, clearance markets and intermediary businesses would contract immediately, incumbent adaptation pipelines would lose their moat, platform takedown policies would lose their doctrinal anchor, and a wave of previously suppressed commercial fan works, mods, and remixes would surface — while rights holders would scramble to rebuild protection through contract and technical measures. The expressive-reuse economy reorganizes around whichever norm replaces it.
% FOUNDING_PROBLEM: Unauthorized commercial recasting of protected expression — translations, dramatizations, adaptations into new media — competing directly with the markets through which rights holders exploited their works, addressed by codifying a derivative-work right in the 1976 Copyright Act.
% FOUNDING_PROBLEM_CORROBORATION: Legislative history (House Report No. 94-1476), leading treatises, and decades of court opinions outside the beneficiary set attest the original adaptation-market rationale and confirm those markets still exist, supporting partial liveness. Rights holders attest the problem is fully live and expanding into new media. Doctrinal scholars and fair-use advocates — also outside the beneficiary set — attest the arrangement now governs conduct (monetized fan works, mods, machine-learning corpora) far beyond the founding problem, supporting the shifted-function reading. Corroboration exists on both sides; neither attests from inside the benefiting parties alone.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__hybrid_carveout_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__hybrid_carveout_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__hybrid_carveout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 at interval end) because the authorization requirement is keyed to a categorical proxy — commerciality — rather than to demonstrated substitution, so some payers harm no exploitable market while paying anyway, and the fee level is set by bargaining position rather than cost. Suppression (0.70) is a raw structural property, unscaled by power or scope: persistence depends on active machinery — statutory damages, DMCA takedowns, platform Content ID — that keeps marginally commercial creators short of the line; the series rises over the interval as automated enforcement matured. Theater ratio (0.28) is low-moderate: licensing and clearance are real functions performed constantly, but a growing share of enforcement activity defends scope expansion (new formats, longer tails, adjacent media) rather than the adaptation markets the arrangement was built for. Accessibility collapse (0.45) is well below mountain range: exits persist — original creation, public-domain and openly licensed sources, staying non-commercial — but none reaches the same expressive material, so alternatives are partial. Resistance (0.50) is sustained: fair-use litigation, doctrinal scholarship, and platform-policy fights contest the line continuously without displacing it. The measurement series run on one shared time grid (interval 0-30, roughly 1994-2024, anchored at the modern commerciality-conscious era of the doctrine); trajectories are monotonic drift, not cyclical, though omega selective_enforcement_cycle flags possible oscillation beneath the trend.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical statutory text. From the copyright_holder seat the arrangement is an orderly licensing market it built and polices — coordination with a price attached. From the commercial_developer seat the same structure operates as a toll gate whose charge varies with commercial status rather than with harm, and whose gray zone is administered by threat. From the noncommercial_creator seat it is a protected commons whose value depends entirely on staying on the permitted side of a line that platform monetization keeps moving toward them. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright_holders sit nearest the beneficiary end: they collect the authorization revenues and control the enforcement agenda (d near 0.0). Licensing_intermediaries likewise benefit, with mobile exit into adjacent rights-administration work. Noncommercial_creators are subsidized by the carveout — the constraint grants them a permission they would otherwise lack — placing them near the beneficiary end despite modest power. Commercial_developers are the declared victims with constrained exit (license, redesign, or litigate), which the derivation places near the full-target end; a directionality override pulls the powerful atom down to 0.6 because the largest commercial actors hold cross-licensing portfolios — major studios and publishers both pay authorization fees and collect them, so their net position is payer-but-partial-collector, not pure target. Open_source_communities straddle: predominantly carveout beneficiaries with episodic exposure when projects monetize. Downstream_audiences bear diffuse indirect costs with mobile exit. Ai_training_developers, excluded from the framework, contest the boundary's reach from outside; their exclusion is enforced by the same doctrine the reading administers.
 *
 * MANDATROPHY ANALYSIS:
 *   Claiming tangled_rope is what keeps both halves of the structure visible. Reading the arrangement as pure rope would hide the categorical extraction: fees keyed to commerciality rather than harm, collected under threat, with the gray zone doing disciplinary work no license ever prices. Reading it as pure snare would erase the real permitted zone: non-commercial transformation is genuinely free in ways that would not survive an enclosure_reading regime, and clearance machinery performs a real matching function for adaptations that do get made. The R5 genealogy supports the hybrid verdict rather than a mandatrophy resolution: the founding problem — unauthorized commercial recasting of protected expression threatening adaptation markets — is still partially live (translation, film, and sequel markets remain real), but the arrangement now governs territories (monetized fan works, mods, reaction formats, machine-learning corpora) far outside it. Status is contested, not dead, so no mandatrophy resolution is declared; the mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges and finds no zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is one reading of the derivative_work_statutory_boundary kernel (reading: hybrid_carveout_reading). How would classification change under the sibling readings enclosure_reading and coordination_reading?',
    'Track doctrinal migration: if courts collapse the commerciality condition and restrict the right to fixed, substantially incorporating recastings, the corpus is drifting toward coordination_reading; if courts extend the right to any incorporative use regardless of commerciality, toward enclosure_reading.',
    'Under enclosure_reading epsilon rises sharply (every transformative use becomes licensable, victims expand to noncommercial creators) and the type trends toward snare; under coordination_reading epsilon falls (only fixed substantial recastings restricted) and the type trends toward rope. The present moderate-epsilon tangled_rope verdict holds only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Committer-frame contingency: classification is indexed to the hybrid carveout reading, not the kernel.').

omega_variable(
    commerciality_harm_decoupling,
    'Does commercial status actually predict harm to the exploitation interests the authorization requirement protects?',
    'Market-substitution studies across the gray zone: monetized fan works, mod distributions, reaction and commentary formats, sampled music — measuring substitution rates against licensing outcomes.',
    'If commerciality and harm are decoupled, the authorization requirement collects fees without serving its incentive rationale and the commercial seat drifts toward pure extraction; if coupled, the coordination half of the structure is confirmed and the tangled_rope reading stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commerciality_harm_decoupling, empirical, 'Whether the commercial/non-commercial trigger tracks the harm the arrangement nominally manages.').

omega_variable(
    commercial_line_indeterminacy,
    'Where exactly does the commercial/non-commercial line sit — ads, tips, patronage, exposure, portfolio-building — and does the indeterminacy itself function as enforcement leverage?',
    'Doctrinal survey of decided cases plus platform-policy analysis of takedown patterns against marginally monetized creators.',
    'A wide unadministered gray zone means effective suppression exceeds the authored scalar: creators self-censor short of the visible line, so measured suppression understates realized chilling. Narrower line-drawing would lower effective suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_line_indeterminacy, conceptual, 'Boundary indeterminacy as a suppression amplifier distinct from the scalar metric.').

omega_variable(
    selective_enforcement_cycle,
    'Does enforcement run in waves — crackdowns on marginally commercial fan creators followed by toleration once platforms normalize — and is the oscillation itself part of how compliance is maintained?',
    'Time-series of takedown volumes and settlement patterns against fan-commercialization waves (comics, fan fiction anthologies, ROM-hack monetization, reaction-video monetization).',
    'If intermittent reinforcement is operating, the flat-to-rising suppression series understates the mechanism: periodic visible enforcement disciplines a much larger population than the enforcement volume alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_cycle, empirical, 'Cyclical selective enforcement as a compliance-maintenance mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__hybrid_carveout_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(deri_tr_t5, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(deri_tr_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(deri_tr_t15, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(deri_tr_t25, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement(deri_tr_t30, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(deri_be_t5, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 5, 0.46).
narrative_ontology:measurement(deri_be_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(deri_be_t15, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(deri_be_t25, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(deri_be_t30, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(deri_su_t5, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 5, 0.44).
narrative_ontology:measurement(deri_su_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(deri_su_t15, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(deri_su_t25, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 25, 0.66).
narrative_ontology:measurement(deri_su_t30, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__hybrid_carveout_reading, resource_allocation).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, coordination_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the derivative work boundary' decomposes into three structurally distinct constraints under the epsilon-invariance principle. enclosure_reading (high epsilon: all incorporative use licensable), coordination_reading (low epsilon: only fixed substantial recastings restricted), and this hybrid_carveout_reading (moderate epsilon: commerciality-keyed restriction with categorical beneficiary split). Each has its own epsilon, victim set, and classification; they are linked here because the upstream readings are cited as authority in disputes over this one — enclosure rhetoric pressures enforcement scope outward, coordination doctrine pressures it inward, and this reading's gray zone is the terrain both contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(derivative_work_statutory_boundary__hybrid_carveout_reading, powerful, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
