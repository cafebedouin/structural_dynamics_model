% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__hybrid_scaffolding_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
 *   human_readable: Scaffolded Imposition of State-Mandated Practice (Hybrid Reading)
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   A consolidating state mandates new public practices — dress and
 *   presentation, civic timekeeping — and the mandate's fate divides sharply
 *   by what rides underneath it. Where the decree stood alone, compliance
 *   collapsed back within a decade: officials kept dual calendars, folk
 *   practice resumed the moment inspectors left. Where the mandate was
 *   scaffolded — urban elites visibly modeling the new practice, a
 *   press-school-ceremony apparatus framing adoption as progress and the old
 *   practice as shame — displacement went partway and stayed: hybrid forms
 *   emerged, adoption ran ahead of enforcement in the cities, and the
 *   countryside complied under duress or evaded. This story authors THAT
 *   scaffolded arrangement as the constraint: the standing regime in which a
 *   practice mandate is held in place by manufactured quasi-endogenous pull.
 *   The ε referent is this standing scaffolded-imposition arrangement,
 *   assessed by this reading's own lights — partial displacement is real
 *   coordination value, and the costs of the transition are borne by those
 *   excluded from the scaffolding. Claim and metrics are independent authored
 *   facts: the claimed type is tangled_rope because the structure holds a
 *   genuine coordination function and an asymmetric extraction channel in one
 *   enforced frame; the metrics describe the arrangement's actual operation
 *   without being tuned to any predicted engine output. KEY AGENTS (by
 *   structural relationship): - reform_state_administration: Agenda setter
 *   (institutional/arbitrage) — drafts, funds, enforces, and could unwind the
 *   arrangement - urban_westernized_elites: Primary beneficiary
 *   (powerful/mobile) — collects status, credentials, and access at minimal
 *   compliance cost - rural_populations: Primary target (powerless/trapped) —
 *   bears fines, exclusion, and enforcement contact without scaffolding
 *   access - traditionalist_notables: Excluded voice with payer costs
 *   (organized/constrained) — loses standing; a co-opted minority is folded
 *   into the framing apparatus - ideological_messaging_producers: Secondary
 *   beneficiary (moderate/mobile) — collects salaries and careers for
 *   manufacturing the pull - comparative_historians: Analytical observer —
 *   attributes the outcome across decree, modeling, and messaging
 *
 * KEY AGENTS:
 *   - reform_state_administration: agenda setter (institutional/arbitrage) — owns the mandate, the enforcement corps, and the amendment path
 *   - urban_westernized_elites: primary beneficiary (powerful/mobile) — status and access economy accrues here at minimal cost
 *   - rural_populations: primary target (powerless/trapped) — bears the mandate's costs with no scaffolding access and no exit
 *   - traditionalist_notables: excluded voice bearing payer costs (organized/constrained) — authority displaced, partially co-opted
 *   - ideological_messaging_producers: secondary beneficiary (moderate/mobile) — manufactures the quasi-endogenous pull for wages
 *   - comparative_historians: analytical observer (analytical/analytical) — holds the paired decree-versus-scaffolding record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.64).
domain_priors:suppression_score(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.7).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "Scaffolded Imposition of State-Mandated Practice (Hybrid Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, '05ee93d0-f2d9-4aa5-9e55-be00d4ef87b0').
narrative_ontology:cs_kernel_codification('05ee93d0-f2d9-4aa5-9e55-be00d4ef87b0', distributed).
narrative_ontology:cs_authority_grounding('05ee93d0-f2d9-4aa5-9e55-be00d4ef87b0', distributed).
narrative_ontology:cs_reading_relation('05ee93d0-f2d9-4aa5-9e55-be00d4ef87b0', legitimacy_of_imposed_practice__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('05ee93d0-f2d9-4aa5-9e55-be00d4ef87b0', legitimacy_of_imposed_practice__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_axiom('05ee93d0-f2d9-4aa5-9e55-be00d4ef87b0', foundational, quasi_endogenous_pull_required_for_displacement).
narrative_ontology:cs_axiom_status(quasi_endogenous_pull_required_for_displacement, holdable).
narrative_ontology:cs_axiom_grounding('05ee93d0-f2d9-4aa5-9e55-be00d4ef87b0', quasi_endogenous_pull_required_for_displacement, empirically_contingent).
narrative_ontology:cs_axiom('05ee93d0-f2d9-4aa5-9e55-be00d4ef87b0', secondary, successful_scaffolding_yields_hybrid_partial_forms).
narrative_ontology:cs_axiom_status(successful_scaffolding_yields_hybrid_partial_forms, holdable).
narrative_ontology:cs_axiom_grounding('05ee93d0-f2d9-4aa5-9e55-be00d4ef87b0', successful_scaffolding_yields_hybrid_partial_forms, empirically_contingent).
narrative_ontology:cs_reference_frame('05ee93d0-f2d9-4aa5-9e55-be00d4ef87b0', scaffolded_convergence_baseline).
narrative_ontology:cs_drift_state('05ee93d0-f2d9-4aa5-9e55-be00d4ef87b0', postcolonial_subaltern_historiography, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('05ee93d0-f2d9-4aa5-9e55-be00d4ef87b0', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_westernized_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, ideological_messaging_producers).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, traditionalist_notables).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, modernization_necessity_doctrine).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, civilizational_parity_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts the mandate, funds the schools, presses, and ceremonial life that demonstrate the new practice, appoints the inspection corps, and collects the fines. It can amend, suspend, or extend the arrangement at cabinet level. It pays the enforcement payroll and the messaging budget, and recoups administrative legibility, fine revenue, and control over public presentation.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, reform_state_administration, agenda_setter,
    institutional, generational, arbitrage, national).

% Adopt the mandated markers early and visibly, staff the ministries, professions, and officer corps that require them, and serve as the modeled exemplars the messaging apparatus circulates. Their fluency in both the old and new registers lets them code-switch, so compliance costs them little while conferring preferential access to state employment, contracts, credit, and urban standing.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_westernized_elites, beneficiary,
    powerful, biographical, mobile, national).

% Live where the old practice is woven into garment economics, ritual timekeeping, and community standing, and far from the schools, newspapers, and urban stages where the new practice is demonstrated and made meaningful. They meet the mandate chiefly as fines, market-town exclusions, and inspector visits. Leaving would mean abandoning land, livelihood, and the community that anchors the old practice.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations, payer,
    powerless, generational, trapped, regional).

% Clergy, guild heads, and provincial gentry whose authority and adjudication income rest on the old practice. They are formally outside the reform conversation — drafts reach them after circulation, consultation is ceremonial. A co-opted minority accepts stipends and lends religious-civilizational vocabulary to the mandate's framing; the remainder lose standing and revenue, and open opposition invites prosecution.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, traditionalist_notables, excluded,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, traditionalist_notables, payer).

% Journalists, schoolteachers, novelists, and ceremony designers employed to render the mandate desirable: progress narratives, ridicule of the old practice, staged accounts of elite exemplars. Their salaries, readerships, and advancement are contingent on the arrangement continuing; their skills transfer poorly to a regime that abandoned the project.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, ideological_messaging_producers, beneficiary,
    moderate, biographical, mobile, national).

% Reconstruct the paired record: the practice imposed by bare decree that collapsed back within a decade, and the practice imposed through mandate plus elite modeling plus ideological framing that achieved partial, hybrid displacement. They attribute shares of the outcome to decree, modeling, and messaging respectively, and hold no stake in the arrangement's continuation.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, comparative_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_westernized_elites).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a single nationally legible surface of public practice — standardized dress and presentation, common civic timekeeping — so that a heterogeneous provincial population becomes administratively readable, mutually recognizable as one polity, and externally legible as a civilized peer to Western powers.
% TRANSFER_FUNCTION: Moves status, state employment, legal protection, and market access toward adopters of the mandated markers, and moves compliance costs — fines, exclusion, harassment, ritual disruption — onto those who cannot or will not adopt, disproportionately rural populations with no access to the scaffolding infrastructure that makes adoption cheap.
% ABSENT_VOICES: Rural populations and the un-co-opted traditionalist notables — the people whose practices are being displaced — were absent from the design conversation. The mandate was drafted by urban elites, for a polity imagined from the capital; rural voices entered the record only as enforcement targets, petitioners after the fact, or defendants in inspection dockets.
% DISAPPEARANCE_RATIONALE: If the scaffolded imposition vanished overnight, the partial displacement already banked would stall and reverse at the margins: hybrid practices would drift back toward prior forms in the countryside, urban elites' identity investments and credential premiums would depreciate, the messaging apparatus would collapse, and the state's legibility gains — uniform census categories, predictable public presentation, common civic calendar — would erode within a generation.
% FOUNDING_PROBLEM: A newly consolidating state faced fragmented provincial populations with diverse local practices that read, to Western capitals and to domestic elites alike, as backwardness inviting external contempt and internal fragmentation. It needed fusion into one nationally legible polity at a pace that gradual diffusion would not deliver.
% FOUNDING_PROBLEM_CORROBORATION: Comparative historians and later post-colonial scholars, writing from outside the benefiting parties, corroborate that the integration and parity problem was live and urgent. Rural oral histories, religious-community ledgers, and inspection-court records corroborate that the displacement costs landed on populations never consulted. No source outside the benefiting parties attests that this particular mechanism — mandate plus manufactured pull — was necessary rather than a slower unmandated diffusion; that necessity claim rests on the reformers' own memoirs and the messaging apparatus itself.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.64 because the arrangement's costs and benefits are systematically misaligned with ability to comply: adoption is cheap where the scaffolding is dense and expensive where it is absent, yet the mandate binds both. Suppression is high (0.70) because persistence depends on active machinery — inspectors, fines, market exclusions, and a continuous messaging budget — not on participant preference; note suppression is authored as a raw structural property and is NOT scaled by power or scope, while extractiveness is the quantity the engine scales through directionality and spatial scope. Theater ratio is moderate-low (0.32): the messaging does real causal work early, but as displacement banks among elites, a growing share of enforcement activity becomes loyalty performance — staged ceremonies, show inspections that mostly net the poor. Accessibility collapse is moderate (0.48) because alternatives never fully close: hybrid forms, rural evasion, and private-sphere continuity persist throughout. Resistance is substantial (0.58): episodic rural unrest, quiet noncompliance, and clerical opposition recur across the interval, and the recurring unrest signals latent coalition capacity among nominally powerless rural actors — individually powerless, they are not without collective leverage, which caps how far suppression can ratchet. The temporal series run on one shared grid (points 0, 5, 10, 15, 20, 25) with every tracked metric authored at every point. The grid and the scalars are one account: aggregate suppression REQUIREMENT rises (the enforcement and messaging machinery is built out and maintained — organizational and structural levels harden), while individual-level experienced coercion FALLS (0.50 to 0.30) as manufactured pull substitutes for fines and show inspections in daily life. That substitution is the reading's core mechanism, and the leveled grid is the only way to represent it without falsifying either half.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very different types from identical structural data. From the rural payer seat — powerless, trapped, near-full target directionality — the arrangement presents as enforced extraction with a thin coordination veneer: fines and exclusions arrive, the schools and presses that would make adoption meaningful do not. From the urban beneficiary seat the same structure presents as a rope it willingly ties: cheap compliance, credential premiums, national belonging. From the agenda-setter seat it presents as a coordination project it built and pays for, with extraction as regrettable friction. The engine computes this divergence from power, exit, and directionality; nothing in the authored claim adjudicates which seat's experience is 'the' constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries map to low directionality: urban_westernized_elites (mobile exit, powerful) sit near the subsidy end — the arrangement lowers their costs and raises their returns; ideological_messaging_producers similarly, with dependence on continuation damping their mobility somewhat. Victims map to high directionality: rural_populations (trapped, powerless) sit near the full-target end — trapped exit pushes them further toward full target than mobile targets would sit. One override is declared: traditionalist_notables hold the organized power atom, and the structural derivation would read pure victim (d near 1.0) from their presence in the victims array; but the co-opted minority receives stipends and mediated standing through the arrangement, so the honest d for the organized seat is approximately 0.72 — predominantly targeted, partially subsidized. The reform_state_administration derives a low-moderate d from its agenda-setter position and arbitrage exit: it pays enforcement costs but nets legibility, revenue, and control.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Reading the arrangement as pure snare would erase the genuine coordination achievement — a nationally legible polity, common civic time, adopters who internalized the practice and would defend it unpaid; the disappearance verdict is world_rearranges precisely because real arrangements depend on it. Reading it as pure rope would erase the extraction channel — a victim class defined by exclusion from the very infrastructure that makes compliance cheap, paying for a transition whose benefits accrued elsewhere. The R5 genealogy sharpens this: the founding problem (fragmentation, external contempt) was live and is corroborated from outside the benefiting parties, but its status is contested because the necessity of THIS mechanism is attested only by the beneficiaries themselves. Founding-problem-status contested crossed with disappearance-verdict world_rearranges yields no zombie flag: the problem has not died and left a hollow arrangement; the argument is over whether the arrangement overshot its problem into rent-taking. The arrangement has nonetheless outlived its original urgency — the parity signal was banked within a decade, while enforcement and messaging budgets persist at steady state — which is the mandatrophy residue the temporal series records as rising theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of kernel legitimacy_of_imposed_practice (reading: hybrid_scaffolding_reading). Which reading''s causal structure should govern classification of the standing imposition arrangement?',
    'Paired natural experiments isolating mandate-only from scaffolded imposition paths, plus convergence of historiographic consensus on mechanism attribution; the sibling readings (exogenous_override_reading, endogenous_climb_reading) are separate constraint files whose verdicts can be compared against this one on the same outcome record.',
    'Under the exogenous reading the victim class dissolves — compliance is a mere legal effect — and the arrangement collapses toward rope. Under the endogenous reading the scaffolding is inert window-dressing and the arrangement inherits only what bottom-up diffusion would have produced alone. The tangled_rope verdict authored here holds specifically under the hybrid reading''s causal structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame routing: this story is one of three readings of the imposed-practice legitimacy kernel; classification is conditional on the reading.').

omega_variable(
    pull_authenticity_ambiguity,
    'Was the quasi-endogenous pull genuine preference shift, or coerced public performance counted as adoption by the enforcement apparatus?',
    'Private-sphere practice records (household inventories, diaries, parish and community registries) cross-checked against public compliance records; divergence between private continuity and public conformity indicates performance masquerading as pull.',
    'If pull is largely performance, the coordination function is thinner than authored, effective extraction is higher than 0.64, and the arrangement shifts toward the snare boundary; if pull is genuine, the rope component is real and the current classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pull_authenticity_ambiguity, empirical, 'Whether manufactured adoption reflects internalized preference or surveilled conformity.').

omega_variable(
    counterfactual_gradualism,
    'Would patient unmandated diffusion have achieved comparable displacement without the extraction channel, making the mandate component pure acceleration purchased with rural costs?',
    'Comparative cases where the same practice diffused without mandate (trade-route dress convergence, commercial timekeeping adoption), measuring displacement depth and lag against the scaffolded case.',
    'If gradual diffusion converges on similar depth within a generation, the mandate''s marginal contribution is speed bought with extraction, tilting the classification toward snare; if diffusion stalls or stratifies, the mandate carries irreducible coordination value, supporting tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_gradualism, empirical, 'Counterfactual contribution of the mandate component versus spontaneous diffusion.').

omega_variable(
    scaffolding_access_extension,
    'Is the victim class a contingent artifact of unequal scaffolding access, such that extending schools, presses, and urban exposure to rural areas would dissolve the extraction asymmetry?',
    'Within-case variation: regions that received disproportionate scaffolding investment versus regions that received mandate enforcement alone, comparing displacement depth and compliance-cost incidence.',
    'If extension dissolves the victim class, the extraction is a distributional defect inside a sound coordination design (rope with a repair path); if rural resistance persists even with full access, the asymmetry tracks identity and livelihood structure, not infrastructure, and the extraction channel is constitutive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffolding_access_extension, empirical, 'Whether the beneficiary/victim split follows scaffolding access or deeper identity structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(loip_hybrid_scaffold_tr_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(loip_hybrid_scaffold_tr_t0, observed).
narrative_ontology:measurement(loip_hybrid_scaffold_tr_t5, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement_basis(loip_hybrid_scaffold_tr_t5, observed).
narrative_ontology:measurement(loip_hybrid_scaffold_tr_t10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(loip_hybrid_scaffold_tr_t10, observed).
narrative_ontology:measurement(loip_hybrid_scaffold_tr_t15, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement_basis(loip_hybrid_scaffold_tr_t15, observed).
narrative_ontology:measurement(loip_hybrid_scaffold_tr_t20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(loip_hybrid_scaffold_tr_t20, observed).
narrative_ontology:measurement(loip_hybrid_scaffold_tr_t25, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 25, 0.32).
narrative_ontology:measurement_basis(loip_hybrid_scaffold_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(loip_hybrid_scaffold_be_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(loip_hybrid_scaffold_be_t0, observed).
narrative_ontology:measurement(loip_hybrid_scaffold_be_t5, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement_basis(loip_hybrid_scaffold_be_t5, observed).
narrative_ontology:measurement(loip_hybrid_scaffold_be_t10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(loip_hybrid_scaffold_be_t10, observed).
narrative_ontology:measurement(loip_hybrid_scaffold_be_t15, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(loip_hybrid_scaffold_be_t15, observed).
narrative_ontology:measurement(loip_hybrid_scaffold_be_t20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(loip_hybrid_scaffold_be_t20, observed).
narrative_ontology:measurement(loip_hybrid_scaffold_be_t25, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 25, 0.64).
narrative_ontology:measurement_basis(loip_hybrid_scaffold_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(loip_hybrid_scaffold_su_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(loip_hybrid_scaffold_su_t0, observed).
narrative_ontology:measurement(loip_hybrid_scaffold_su_t5, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(loip_hybrid_scaffold_su_t5, observed).
narrative_ontology:measurement(loip_hybrid_scaffold_su_t10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(loip_hybrid_scaffold_su_t10, observed).
narrative_ontology:measurement(loip_hybrid_scaffold_su_t15, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(loip_hybrid_scaffold_su_t15, observed).
narrative_ontology:measurement(loip_hybrid_scaffold_su_t20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(loip_hybrid_scaffold_su_t20, observed).
narrative_ontology:measurement(loip_hybrid_scaffold_su_t25, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(loip_hybrid_scaffold_su_t25, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=25
narrative_ontology:measurement(loip_hybrid_scaffold_grid_01, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse(class), 0, 0.3).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_02, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse(class), 25, 0.4).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_03, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_04, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse(individual), 25, 0.6).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_05, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse(organizational), 0, 0.4).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_06, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse(organizational), 25, 0.7).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_07, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse(structural), 0, 0.45).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_08, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse(structural), 25, 0.65).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_09, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance(class), 0, 0.6).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance(class), 25, 0.55).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_11, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance(individual), 0, 0.55).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_12, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance(individual), 25, 0.3).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_13, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance(organizational), 0, 0.35).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_14, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance(organizational), 25, 0.2).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_15, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance(structural), 0, 0.45).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_16, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance(structural), 25, 0.4).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_17, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, stakes_inflation(class), 0, 0.4).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_18, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, stakes_inflation(class), 25, 0.6).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_19, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, stakes_inflation(individual), 0, 0.45).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, stakes_inflation(individual), 25, 0.55).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_21, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, stakes_inflation(organizational), 0, 0.5).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_22, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, stakes_inflation(organizational), 25, 0.65).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_23, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, stakes_inflation(structural), 0, 0.5).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_24, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, stakes_inflation(structural), 25, 0.7).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_25, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression(class), 0, 0.4).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_26, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression(class), 25, 0.55).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_27, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression(individual), 0, 0.5).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_28, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression(individual), 25, 0.3).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_29, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression(organizational), 0, 0.45).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_30, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression(organizational), 25, 0.6).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_31, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression(structural), 0, 0.5).
narrative_ontology:measurement(loip_hybrid_scaffold_grid_32, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression(structural), 25, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'does top-down imposition of practice work?' decomposes into three structurally distinct claims, each authored as its own story over the same standing arrangement. This file (hybrid_scaffolding_reading) authors the middle claim: mandate succeeds only when ideological reinforcement generates quasi-endogenous pull, yielding partial hybrid displacement. The exogenous_override_reading authors decree-sufficiency (its epsilon reflects a legal-effect view of compliance with no victim class beyond ordinary legal incidence); the endogenous_climb_reading authors internalization-necessity (its epsilon reflects scaffolding as inertial overhead atop a bottom-up process). This reading cites both limiting cases as evidence — the decree-only collapse and the slow unaided climb — which is why the upstream limiting readings influence this one while this one's outcome record feeds back pressure on both. Each member carries its own epsilon, beneficiaries, and victims; none averages across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
