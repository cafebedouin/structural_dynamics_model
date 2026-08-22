% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deliberative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__deliberative_reading, []).

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
 *   constraint_id: notability_guidelines__deliberative_reading
 *   human_readable: Notability as Perpetual Deliberative Boundary Process (AfD Negotiation Reading)
 *   domain: digital_commons_governance/knowledge_infrastructure/platform_constitutionalism
 *
 * SUMMARY:
 *   This story instantiates the deliberative reading of the notability
 *   kernel: the standing arrangement under examination is the procedure
 *   itself - any editor may nominate an article for deletion, any editor may
 *   defend it, uninvolved administrators close each discussion by weighing
 *   arguments against a guideline text that is itself continuously revised
 *   through community request-for-comment. On this reading, notability is not
 *   a fixed criterion applied to the world; it is the running output of an
 *   adversarial-deliberative process, with each closure provisional,
 *   precedents accumulating informally rather than binding, and the boundary
 *   expected to move as the community learns. The constraint's justification
 *   is the negotiation, not any particular line it currently draws, and every
 *   individual determination carries its own termination point - which is the
 *   sense in which this story claims a sunset clause. KEY AGENTS (by
 *   structural relationship): closing_administrators (institutional/mobile) -
 *   administer closures and hold interpretive discretion;
 *   encyclopedia_readers (powerless/mobile) - diffuse beneficiary of the
 *   credibility signal; active_editor_community (organized/identity_locked) -
 *   dual-positioned beneficiary-payer staffing the process;
 *   first_time_article_authors (powerless/mobile) - principal cost-bearers;
 *   niche_field_experts (moderate/constrained) - cost-bearers with partial
 *   recourse; undercovered_region_communities (powerless/constrained) -
 *   excluded voices bearing the boundary's blind spots; wikipedia_researchers
 *   (analytical/analytical) - observers of the whole structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deliberative_reading, 0.34).
domain_priors:suppression_score(notability_guidelines__deliberative_reading, 0.32).
domain_priors:theater_ratio(notability_guidelines__deliberative_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deliberative_reading, scaffold).
narrative_ontology:human_readable(notability_guidelines__deliberative_reading, "Notability as Perpetual Deliberative Boundary Process (AfD Negotiation Reading)").
narrative_ontology:topic_domain(notability_guidelines__deliberative_reading, "digital_commons_governance/knowledge_infrastructure/platform_constitutionalism").

domain_priors:requires_active_enforcement(notability_guidelines__deliberative_reading).
narrative_ontology:has_sunset_clause(notability_guidelines__deliberative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deliberative_reading, 'be2cf378-10bc-4e10-8c1e-c75039eb18a1').
narrative_ontology:cs_kernel_codification('be2cf378-10bc-4e10-8c1e-c75039eb18a1', distributed).
narrative_ontology:cs_authority_grounding('be2cf378-10bc-4e10-8c1e-c75039eb18a1', practice).
narrative_ontology:cs_interpretation_layer_present('be2cf378-10bc-4e10-8c1e-c75039eb18a1').
narrative_ontology:cs_reading_relation('be2cf378-10bc-4e10-8c1e-c75039eb18a1', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('be2cf378-10bc-4e10-8c1e-c75039eb18a1', notability_guidelines__inclusionist_reading, influences).
narrative_ontology:cs_axiom('be2cf378-10bc-4e10-8c1e-c75039eb18a1', foundational, boundary_as_deliberative_output).
narrative_ontology:cs_axiom_status(boundary_as_deliberative_output, holdable).
narrative_ontology:cs_axiom_grounding('be2cf378-10bc-4e10-8c1e-c75039eb18a1', boundary_as_deliberative_output, conventional).
narrative_ontology:cs_axiom('be2cf378-10bc-4e10-8c1e-c75039eb18a1', foundational, case_by_case_adjudication).
narrative_ontology:cs_axiom_status(case_by_case_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('be2cf378-10bc-4e10-8c1e-c75039eb18a1', case_by_case_adjudication, instrumental).
narrative_ontology:cs_reference_frame('be2cf378-10bc-4e10-8c1e-c75039eb18a1', perpetual_consensus_negotiation).
narrative_ontology:cs_drift_state('be2cf378-10bc-4e10-8c1e-c75039eb18a1', contemporary_routinization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('be2cf378-10bc-4e10-8c1e-c75039eb18a1', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deliberative_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, encyclopedia_readers).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, active_editor_community).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, first_time_article_authors).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, niche_field_experts).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, undercovered_region_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, niche_field_experts).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, active_editor_community).
narrative_ontology:constraint_vindicates(notability_guidelines__deliberative_reading, procedural_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(notability_guidelines__deliberative_reading, consensus_epistemology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Volunteer administrators who close deletion discussions, weigh participant arguments against the current guideline text, and write closure rationales. They exercise day-to-day discretion over what the boundary means in each case, accumulate community standing through closure work, and absorb a heavy caseload. Any of them can retire or simply stop closing; the process continues with whoever remains.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, closing_administrators, agenda_setter,
    institutional, biographical, mobile, global).

% Consume the coverage boundary the process produces but almost never observe or participate in the deliberation that draws it. They receive a credibility signal - the assurance that covered topics met some communal threshold - and bear the cost of gaps in coverage only invisibly, as things they cannot find. Their exit is trivial: read something else.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, encyclopedia_readers, beneficiary,
    powerless, immediate, mobile, global).

% Long-tenure editors who staff the discussions, cite and contest precedent, and treat the negotiation arena as a primary social world. They benefit from governing a legitimate collective process and from the reputational capital earned inside it; they pay in governance labor, in articles they valued being deleted, and in the emotional cost of losing arguments. Leaving would mean abandoning a community their editing identity is constituted through, which is why departures are rare and usually framed as sabbaticals.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, active_editor_community, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__deliberative_reading, active_editor_community, payer).

% Create an article, often their first edit of substance, and can face a deletion nomination within days. They encounter policy jargon, uninvolved participants applying standards they have never read, and a closure that removes their work. Most never return after a deletion; their exit is silent attrition rather than engagement.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, first_time_article_authors, payer,
    powerless, immediate, mobile, global).

% Specialists whose fields are documented in scholarly literature but thin in general-audience press, so their topics recurrently fail significance arguments built on mainstream-source expectations. They lose articles they are uniquely positioned to verify, spend effort defending them, and possess external careers that soften the blow - but the encyclopedia is the public interface of their field, so walking away concedes the representation of their subject to others.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, niche_field_experts, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__deliberative_reading, niche_field_experts, beneficiary).

% Communities whose histories, figures, and events are underdocumented in the independent reliable sources the significance tests privilege. They would object that the boundary encodes Anglophone press availability rather than subject importance, but they are largely absent from the discussion spaces where the boundary is argued. Alternative distribution channels for their knowledge exist but reach nothing like the audience of the reference layer of the web.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, undercovered_region_communities, excluded,
    powerless, generational, constrained, regional).

% Academic and independent researchers studying peer production, knowledge infrastructures, and governance. They observe closure patterns, participation demographics, and boundary movement across the whole archive of discussions, and publish analyses no seat inside the process commissioned.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, wikipedia_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__deliberative_reading, diffuse).
narrative_ontology:fixing_cost_class(notability_guidelines__deliberative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates finite editorial attention, page space, and collective credibility across an effectively infinite universe of candidate topics without any central authority empowered to decide - the discussion-and-closure procedure lets thousands of dispersed volunteers settle, case by case, which subjects the commons covers and which it does not.
% TRANSFER_FUNCTION: Moves content decisions from individual article authors to the deliberating community; moves deletion risk disproportionately onto newcomers and niche-topic authors; moves interpretive authority and community standing toward the experienced participants and closers who run the procedure.
% ABSENT_VOICES: Casual readers, undercovered-region knowledge communities, and future researchers who inherit today's coverage gaps would object if present; they are absent because the discussion venues demand policy fluency, available daytime hours in Euro-American time zones, and tolerance for adversarial argument - a self-selection filter the process does not correct for.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, tens of thousands of pending nominations and speedy-deletion tags would lose their resolution path, WikiProject rescue workflows and draft-space pipelines built around the process would strand, the credibility signal readers rely on would wobble until a replacement boundary mechanism emerged, and the editor community's central argumentative arena would disappear - the governance layer of the encyclopedia would visibly reorganize.
% FOUNDING_PROBLEM: Early Wikipedia faced indiscriminate inclusion: vanity pages, hoax biographies, and promotional articles threatened both credibility and the tractability of the growing corpus, and the community needed a minimum line below which topics would not be covered.
% FOUNDING_PROBLEM_CORROBORATION: That the founding problem was real is corroborated from outside the benefiting parties: early mailing-list archives, contemporaneous press criticism of Wikipedia reliability, and the pre-guideline deletion wars of 2004-2006 are matters of record, and longitudinal article-quality research attests the filter's measurable effect. Whether the problem remains live is disputed between camps - deletionist-leaning editors cite continuing junk floods at scale, while inclusionist-leaning researchers cite systematic coverage-gap data - and no party outside the dispute currently holds decisive evidence either way.
narrative_ontology:disappearance_verdict(notability_guidelines__deliberative_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deliberative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deliberative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(notability_guidelines__deliberative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deliberative_reading, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__deliberative_reading_tests).
:- end_tests(notability_guidelines__deliberative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored moderate-low (0.34 at interval end) because the arrangement's costs - destroyed author labor, defensive effort, coverage gaps - are real but bounded by revisability: closures can be appealed, drafts preserved, and the guideline text itself amended. Suppression (0.32) is authored as a raw structural property, unscaled by power or scope in the engine's computation: deletion is coercive against the author's wishes, but exits (userification, draft space, recreation after improvement, deletion review) remain partly open, so the constraint does not fully foreclose alternatives - hence accessibility_collapse 0.45, well below mountain range. Resistance (0.55) reflects sustained inclusionist pushback, mass-defense efforts, and recurring reform campaigns. Theater_ratio (0.22) captures the ritualized share of discussion activity - boilerplate policy citations, significance-test incantations, relitigated arguments - against the share doing genuine evaluative work. The temporal series run on ONE shared grid (t=0..24, one unit per year, mapping approximately 2001-2025) with all three metrics authored at every point. The series document a full cycle: early informal deletion (high arbitrariness, low machinery), formalization and speedy-deletion expansion, a deletionist ascendancy peaking around t=8-12 with maximum enforcement intensity, then reform waves - proposed deletion, draft space, article creation workflows - relaxing enforcement while extraction and theater recede. The cycle driver is crisis-reform-relaxation-accumulation: inclusion surges produce junk crises, hardening follows, collateral deletions of legitimate work produce backlash, reforms divert routine cases, and accumulation resumes. The oscillation is partly functional error-correction, but its hardening phases concentrate costs on newcomers in an intermittent-reinforcement pattern, and the end-state base_properties values are measured at t=24, a post-reform relaxation phase - a reader comparing the scalar values to the series peak should note the phase.
 *
 * PERSPECTIVAL GAP:
 *   The closer seat and the author seat should compute differently from identical structural data. From closing_administrators' position the arrangement is a legitimate collective instrument they operate with care; from first_time_article_authors' position the same closure is an opaque tribunal that destroyed their work within days of creation. Same-level divergence appears inside the nominal editor class: active_editor_community and first_time_article_authors are both 'editors,' but the veterans hold organized power, policy fluency, and identity-locked exit (their social world is the project), while newcomers hold no organization, no fluency, and mobile exit that resolves into silent attrition - equal nominal standing, radically different experienced constraints. Niche_field_experts diverge from both: external careers soften extraction, but field representation stakes make exit costly in a way neither group faces. The Wikimedia Foundation sits structurally above this entire arrangement - it hosts the platform but delegates content governance wholesale - so no corporate seat appears among the stakeholders; its absence from the deliberation is itself a structural fact the researcher seat observes.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for encyclopedia_readers (pure beneficiary, diffuse gain, trivial exit) and net-low for active_editor_community, whose dual beneficiary-payer position nets positive through standing and governance legitimacy. Victim declarations drive high directionality for first_time_article_authors (full cost, no compensation, no voice) and moderately high for niche_field_experts and undercovered_region_communities, the latter amplified by their exclusion from the conversation that sets the boundary affecting them. closing_administrators derive a near-symmetric d: they collect standing and discretion but pay heavy workload and bear responsibility for contested closures, and their exit (retirement) is genuinely open. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already place each seat correctly, and the schema's override mechanism keys on power atoms, which would collide across the multiple distinct agents sharing the powerless atom here.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold claim disciplines both mislabeling directions. Against the snare reading: the process's coordination function - allocating attention and credibility without central authority - is genuine, its extraction is bounded by per-case appeal and continuous guideline revision, and its enforcement intensity has historically RECeded under reform rather than ratcheting, which a pure extraction mechanism does not do. Against the rope reading: the arrangement's justification is transitional calibration toward a better-tracked boundary, not a steady state - if the negotiation converged, the adjudication machinery would be needed ever more rarely, and a 'perpetual' process that never converges is carrying something beyond its declared function. The R5 interview locates the residual risk precisely: founding_problem_status is contested and disappearance_verdict is world_rearranges, so the mismatch consumer finds no dead-problem-plus-dependence signature; the live worry is instead carried by the routinization omegas - if precedent-weighting has replaced fresh deliberation, the process persists theatrically around a function it no longer performs, and the scaffold degrades toward piton without any seat profiting enough to fix it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the notability_guidelines kernel - the deliberative reading, on which the boundary is an output of AfD negotiation rather than an input the guideline fixes. Which structural facts would change if a sibling reading were adopted instead?',
    'Compare compiled stories across the kernel family: the deletionist_reading authors the filter''s necessity as the operative fact; the inclusionist_reading authors systematic exclusion as the operative fact. The disagreement is located in the input/output status of the boundary line, and each reading yields a different victim set and epsilon over the same referent arrangement.',
    'Adopting the deletionist reading raises authored suppression and re-centers targets on bad-faith content creators; adopting the inclusionist reading raises extraction substantially and converts undercovered_region_communities from excluded cost-bearers into systematic victims. This story''s classification holds only under the deliberative reading''s commitments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story instantiates one of three declared readings of the notability kernel; sibling readings instantiate different constraints.').

omega_variable(
    scaffold_transition_convergence,
    'Is the negotiation genuinely transitional - converging, case by case, toward a stably tracked boundary - or permanently self-perpetuating without convergence?',
    'Longitudinal analysis of the discussion archive: measure whether significance-test interpretations drift measurably decade over decade, and what fraction of closed determinations are later reopened, overturned at deletion review, or effectively reversed by guideline amendment.',
    'Demonstrable convergence supports the scaffold claim and its sunset structure; persistent non-convergence with stable machinery indicates the process maintains itself beyond its declared transition, pushing classification toward tangled_rope or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_transition_convergence, empirical, 'Whether the deliberative process converges or perpetuates itself indefinitely.').

omega_variable(
    deliberative_body_composition,
    'Does the population actually doing the deliberating represent the constituencies the boundary affects - readers, niche experts, undercovered communities - or a self-selected cadre of policy-fluent regulars?',
    'Participation demographics from the discussion archive compared against reader demographics and affected-population estimates; attendance patterns at high-stakes requests for comment versus routine discussions.',
    'If a cadre dominates, the transfer function skews toward insider interests, the authored extraction understates what outsiders experience, and the excluded-seat costs migrate into the computed extraction for every powerless target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberative_body_composition, empirical, 'Representativeness of the deliberating population relative to affected constituencies.').

omega_variable(
    routinization_vs_maturation,
    'Is the heavy precedent-weighting observed in contemporary closures a degradation of deliberation into rote citation, or its maturation into efficient doctrine?',
    'Content analysis of closure rationales across the interval: ratio of citations to prior outcomes versus engagement with case-specific evidence, correlated with reversal rates on review.',
    'If routinization dominates, theater_ratio is understated at interval end and the practice_drift magnitude in cs_structure should read severe; if maturation, the drift is benign adaptation and the scaffold reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(routinization_vs_maturation, conceptual, 'Whether precedent-weighting constitutes decay or consolidation of the deliberative function.').

omega_variable(
    per_case_vs_meta_sunset_scope,
    'Does the sunset structure claimed by this story - per-case closure endpoints plus explicit revisability - satisfy what scaffold classification requires, or does scaffold demand a meta-level termination condition the perpetual process lacks?',
    'Conceptual analysis against the scaffold definition: determine whether a constraint whose every output is provisional but whose machinery declares no terminal state counts as carrying a sunset clause, or whether the clause must bind the arrangement itself.',
    'If the meta-level reading governs, the has_sunset_clause declaration fails and the claimed type must fall to tangled_rope (coordination plus asymmetric newcomer costs under active enforcement) pending evidence of convergence; if the per-case reading governs, the scaffold claim stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(per_case_vs_meta_sunset_scope, conceptual, 'Scope ambiguity in the sunset-clause requirement as applied to a perpetual process.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deliberative_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wpn_deliberative_tr_t0, notability_guidelines__deliberative_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(wpn_deliberative_tr_t0, observed).
narrative_ontology:measurement(wpn_deliberative_tr_t4, notability_guidelines__deliberative_reading, theater_ratio, 4, 0.19).
narrative_ontology:measurement_basis(wpn_deliberative_tr_t4, observed).
narrative_ontology:measurement(wpn_deliberative_tr_t8, notability_guidelines__deliberative_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement_basis(wpn_deliberative_tr_t8, observed).
narrative_ontology:measurement(wpn_deliberative_tr_t12, notability_guidelines__deliberative_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement_basis(wpn_deliberative_tr_t12, observed).
narrative_ontology:measurement(wpn_deliberative_tr_t16, notability_guidelines__deliberative_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement_basis(wpn_deliberative_tr_t16, observed).
narrative_ontology:measurement(wpn_deliberative_tr_t20, notability_guidelines__deliberative_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(wpn_deliberative_tr_t20, observed).
narrative_ontology:measurement(wpn_deliberative_tr_t24, notability_guidelines__deliberative_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement_basis(wpn_deliberative_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(wpn_deliberative_be_t0, notability_guidelines__deliberative_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(wpn_deliberative_be_t0, observed).
narrative_ontology:measurement(wpn_deliberative_be_t4, notability_guidelines__deliberative_reading, base_extractiveness, 4, 0.36).
narrative_ontology:measurement_basis(wpn_deliberative_be_t4, observed).
narrative_ontology:measurement(wpn_deliberative_be_t8, notability_guidelines__deliberative_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement_basis(wpn_deliberative_be_t8, observed).
narrative_ontology:measurement(wpn_deliberative_be_t12, notability_guidelines__deliberative_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement_basis(wpn_deliberative_be_t12, observed).
narrative_ontology:measurement(wpn_deliberative_be_t16, notability_guidelines__deliberative_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement_basis(wpn_deliberative_be_t16, observed).
narrative_ontology:measurement(wpn_deliberative_be_t20, notability_guidelines__deliberative_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement_basis(wpn_deliberative_be_t20, observed).
narrative_ontology:measurement(wpn_deliberative_be_t24, notability_guidelines__deliberative_reading, base_extractiveness, 24, 0.34).
narrative_ontology:measurement_basis(wpn_deliberative_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(wpn_deliberative_su_t0, notability_guidelines__deliberative_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(wpn_deliberative_su_t0, observed).
narrative_ontology:measurement(wpn_deliberative_su_t4, notability_guidelines__deliberative_reading, suppression_requirement, 4, 0.34).
narrative_ontology:measurement_basis(wpn_deliberative_su_t4, observed).
narrative_ontology:measurement(wpn_deliberative_su_t8, notability_guidelines__deliberative_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement_basis(wpn_deliberative_su_t8, observed).
narrative_ontology:measurement(wpn_deliberative_su_t12, notability_guidelines__deliberative_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement_basis(wpn_deliberative_su_t12, observed).
narrative_ontology:measurement(wpn_deliberative_su_t16, notability_guidelines__deliberative_reading, suppression_requirement, 16, 0.43).
narrative_ontology:measurement_basis(wpn_deliberative_su_t16, observed).
narrative_ontology:measurement(wpn_deliberative_su_t20, notability_guidelines__deliberative_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement_basis(wpn_deliberative_su_t20, observed).
narrative_ontology:measurement(wpn_deliberative_su_t24, notability_guidelines__deliberative_reading, suppression_requirement, 24, 0.32).
narrative_ontology:measurement_basis(wpn_deliberative_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deliberative_reading, resource_allocation).
narrative_ontology:affects_constraint(notability_guidelines__deliberative_reading, notability_guidelines__deletionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deliberative_reading, notability_guidelines__inclusionist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'WP:N' decomposes into three structurally distinct constraints - one per declared reading of the notability_guidelines kernel - per the epsilon-invariance principle. Each family member carries its own epsilon, victim set, and claimed type over the same referent arrangement. The deletionist reading is the historically upstream member (its filter-necessity premise motivated the guideline's creation and is cited as evidence within the other two); this deliberative reading sits mid-family, and its demonstrated boundary-movement creates downstream pressure on the inclusionist reading's operating environment without resolving whether the system is systematically biased. All three files link one another through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
