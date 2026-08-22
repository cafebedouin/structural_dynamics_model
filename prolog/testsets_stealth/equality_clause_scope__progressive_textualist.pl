% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__progressive_textualist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__progressive_textualist, []).

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
 *   constraint_id: equality_clause_scope__progressive_textualist
 *   human_readable: Equality Clause Scope — Progressive Textualist Reading (Amendment-Gated Expansion)
 *   domain: constitutional/political/civil_rights_history
 *
 * SUMMARY:
 *   A constitutional equality guarantee whose application scope is bounded by
 *   the text as duly ratified and widened only through the formal amendment
 *   process: courts construe the enacted text but may not extend its
 *   application beyond what supermajority enactment has carried it. The
 *   arrangement solves a real coordination problem — it gives a continental
 *   republic a single, democratically authored rule for when the guarantee's
 *   reach changes, stabilizing expectations for citizens, legislatures, and
 *   courts alike — while imposing asymmetric costs: groups the currently
 *   ratified scope does not reach bear the full burden of exclusion until the
 *   electorates the current scope protects consent, by supermajority, to
 *   include them. This file instantiates one reading of a decomposed kernel
 *   (see commentary.kernel_context and network.dual_formulation_note);
 *   epsilon is authored for this reading's arrangement only, assessed by the
 *   reading's own lights. The claim/metric relationship is deliberately
 *   unreconciled: the reading CLAIMS the amendment gate as legitimate bounded
 *   universalism while the authored metrics describe its actual mixed
 *   operation — a genuine coordination channel that also enforces real
 *   exclusion — and the engine computes per-seat classifications from the
 *   structural data.
 *
 * KEY AGENTS:
 *   - out_of_scope_groups: Primary target (powerless/trapped) — bears the costs of exclusion pending supermajority consent they cannot self-author
 *   - incumbent_scope_members: Primary beneficiary (organized/constrained) — the current scope's protected constituency; expansion proceeds only at their consent
 *   - constitutional_amendment_institutions: Agenda setter (institutional/constrained) — Congress and the state legislatures operating the sole legitimate widening channel
 *   - federal_judiciary: Boundary administrator (institutional/constrained) — applies the clause at ratified scope and declines to move it; collects interpretive authority, pays legitimacy costs
 *   - equality_litigants: Secondary target (moderate/constrained) — absorbs the costs of claims the framework treats as categorically out of bounds
 *   - living_constitution_advocates: Excluded (organized/constrained) — methodology barred from the decision procedure though vocal in public discourse
 *   - constitutional_theorists: Analytical observer (analytical/analytical) — comparative amendment-difficulty scholarship; no stake in which scope prevails
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, 0.48).
domain_priors:suppression_score(equality_clause_scope__progressive_textualist, 0.58).
domain_priors:theater_ratio(equality_clause_scope__progressive_textualist, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, extractiveness, 0.48).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__progressive_textualist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__progressive_textualist, "Equality Clause Scope — Progressive Textualist Reading (Amendment-Gated Expansion)").
narrative_ontology:topic_domain(equality_clause_scope__progressive_textualist, "constitutional/political/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__progressive_textualist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__progressive_textualist, 'c4344ef7-2ab6-4505-896c-2980bb0c13ff').
narrative_ontology:cs_kernel_codification('c4344ef7-2ab6-4505-896c-2980bb0c13ff', fixed_text).
narrative_ontology:cs_authority_grounding('c4344ef7-2ab6-4505-896c-2980bb0c13ff', lineage).
narrative_ontology:cs_interpretation_layer_present('c4344ef7-2ab6-4505-896c-2980bb0c13ff').
narrative_ontology:cs_reading_relation('c4344ef7-2ab6-4505-896c-2980bb0c13ff', equality_clause_scope__restrictive_originalist, forecloses).
narrative_ontology:cs_reading_relation('c4344ef7-2ab6-4505-896c-2980bb0c13ff', equality_clause_scope__expansive_universalist, forecloses).
narrative_ontology:cs_axiom('c4344ef7-2ab6-4505-896c-2980bb0c13ff', foundational, text_contains_generalizable_equality_principle).
narrative_ontology:cs_axiom_status(text_contains_generalizable_equality_principle, holdable).
narrative_ontology:cs_axiom_grounding('c4344ef7-2ab6-4505-896c-2980bb0c13ff', text_contains_generalizable_equality_principle, deontological).
narrative_ontology:cs_axiom('c4344ef7-2ab6-4505-896c-2980bb0c13ff', foundational, scope_expansion_requires_supermajority_democratic_consent).
narrative_ontology:cs_axiom_status(scope_expansion_requires_supermajority_democratic_consent, holdable).
narrative_ontology:cs_axiom_grounding('c4344ef7-2ab6-4505-896c-2980bb0c13ff', scope_expansion_requires_supermajority_democratic_consent, conventional).
narrative_ontology:cs_axiom('c4344ef7-2ab6-4505-896c-2980bb0c13ff', secondary, judicial_reinterpretation_cannot_widen_application_scope).
narrative_ontology:cs_axiom_status(judicial_reinterpretation_cannot_widen_application_scope, holdable).
narrative_ontology:cs_axiom_grounding('c4344ef7-2ab6-4505-896c-2980bb0c13ff', judicial_reinterpretation_cannot_widen_application_scope, conventional).
narrative_ontology:cs_reference_frame('c4344ef7-2ab6-4505-896c-2980bb0c13ff', ratified_text_through_duly_enacted_amendment).
narrative_ontology:cs_drift_state('c4344ef7-2ab6-4505-896c-2980bb0c13ff', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c4344ef7-2ab6-4505-896c-2980bb0c13ff', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__progressive_textualist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, incumbent_scope_members).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, constitutional_amendment_institutions).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, out_of_scope_groups).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, equality_litigants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, federal_judiciary).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, article_five_exclusivity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Congress and the state legislatures jointly operate the only legitimate channel for widening the clause's application: two-thirds proposal, three-fourths ratification. Every inclusion claim must pass through their calendars and coalitions, converting demands for equal treatment into supermajority politics. They cannot bypass the procedure they administer, but they set its pace and collect the agenda leverage that comes from being its doorkeepers.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, constitutional_amendment_institutions, agenda_setter,
    institutional, generational, constrained, national).

% People already holding the clause's protections under the currently ratified scope. Every expansion question is decided by electorates in which they are the numerous, enfranchised side; the existing distribution of rights and obligations persists until they consent otherwise. Their position does not depend on exiting anything — they are the arrangement's constituency, and the delay of any expansion accrues to them as continued possession of the status quo.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, incumbent_scope_members, beneficiary,
    organized, biographical, constrained, national).

% Groups the clause's currently ratified scope does not yet reach. They carry the full weight of exclusion — unprotected interests, second-class legal status — for as long as supermajority consent is withheld, and they cannot grant themselves inclusion: they must assemble votes from the very constituencies the current scope protects. Leaving the jurisdiction or retreating to whatever state-level protection exists are the only exits, both costly and partial. Whole biographies pass inside the gap between the principle announced and the scope enacted.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, out_of_scope_groups, payer,
    powerless, biographical, trapped, national).

% Plaintiffs and movement lawyers who bring equality claims. Within this framework courts may construe the enacted text but may not widen its application, so claims requiring reach beyond the ratified scope fail regardless of their merit — dismissed as categorically out of bounds rather than weighed. The litigants absorb the costs of those failures and redirect their efforts toward the amendment channel or in-scope arguments.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, equality_litigants, payer,
    moderate, biographical, constrained, national).

% Courts police the boundary daily: they apply the clause at its ratified scope, dismiss claims that would move it, and decline invitations to accomplish by interpretation what the amendment process reserves to enactment. Their institutional authority rests on being the exclusive interpreter of a boundary they may not move; they bear the public legitimacy cost of every denial that other frameworks would call an injustice, and life tenure insulates them from direct consequence while not from the accumulated cost.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, federal_judiciary, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__progressive_textualist, federal_judiciary, beneficiary).

% Scholars, advocates, and jurists who hold that the clause's application should track evolving public understanding through judicial construction. Within this framework their methodology is procedurally barred — not refuted argument by argument but ruled out as a category of authority — so their objections register loudly in public discourse while carrying no weight inside the decision procedure itself. Their practical recourse is persuasion of the electorates whose consent the framework requires.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, living_constitution_advocates, excluded,
    organized, generational, constrained, national).

% Comparative constitutional scholars and amendment-difficulty researchers who study how amendment thresholds shape the incidence of constitutional change across many polities. They take testimony from every seat, publish analyses no seat controls, and hold no stake in which scope prevails; their work supplies the outside evidence base for evaluating whether the threshold filters or entrenches.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, constitutional_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__progressive_textualist, incumbent_scope_members).
narrative_ontology:fixing_cost_class(equality_clause_scope__progressive_textualist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, democratically authored rule for when the equality guarantee's reach changes in a large federal republic: supermajority enactment stabilizes expectations for citizens, legislatures, and courts, prevents case-by-case redetermination of the boundary, and blocks transient factions from rewriting fundamental terms while preserving a correction channel the founders sized against the Articles of Confederation's unanimity trap.
% TRANSFER_FUNCTION: Moves scope-setting authority over equal citizenship from courts and litigating minorities to supermajority coalitions of the existing electorate; moves the costs of exclusion (delayed protection, second-class status) onto out-of-scope groups for exactly as long as that consent is withheld; and moves agenda leverage over every inclusion claim to the amendment institutions that operate the gate.
% ABSENT_VOICES: At drafting, the enslaved and women — the clause's largest eventual claimant classes — had no voice at all. Today, out-of-scope groups vote but cannot self-author their inclusion: they are present in the electorate yet structurally dependent on votes from those the current scope protects. Future generations bound by today's ratified scope are absent entirely. Advocates of judicial expansion are present in discourse but procedurally excluded from the decision itself.
% DISAPPEARANCE_RATIONALE: If the amendment-gate vanished overnight, scope-setting authority would migrate immediately to whichever institution moved fastest — the courts — and every landmark expansion and denial would become contestable case by case. The supermajority veto would disappear, out-of-scope groups' fortunes would decouple from electoral coalition-building, the amendment institutions would lose their gatekeeping leverage, and the distribution of equal-citizenship protections would rearrange around judicial doctrine rather than enacted text.
% FOUNDING_PROBLEM: Build a supreme law that could bind future majorities without being hostage to ordinary politics: a change threshold high enough to prevent factional rewriting of fundamental terms, low enough to permit correction — sized directly against the failure of the Articles of Confederation, whose unanimity rule made amendment practically impossible.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the ratification-era debates (Federalist and anti-Federalist alike, written by participants with opposing stakes) record the threshold tradeoff as the explicit design question; comparative constitutional scholarship on amendment difficulty attests the problem is live in every polity that faces it; and civil-rights historiography — written largely outside the arrangement's beneficiary set — documents that excluded groups themselves carried the 13th, 14th, 15th, and 19th Amendments through the channel, attesting both that the founding problem persists and that the mechanism addresses it. No serious source contends the problem is dead; the dispute is over whether the current threshold still balances it well.
narrative_ontology:disappearance_verdict(equality_clause_scope__progressive_textualist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__progressive_textualist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__progressive_textualist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equality_clause_scope__progressive_textualist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__progressive_textualist, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__progressive_textualist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__progressive_textualist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.48 at interval end) because the exclusion costs are real and concentrated — an out-of-scope group waits entire biographies for a supermajority — while the reading's own assessment credits the mechanism as the legitimate price of democratic authorship; the referent is the standing amendment-gated arrangement, never the universalist alternative. Suppression (0.58) is predominantly structural: the judicial route is procedurally foreclosed rather than argued away, and claims beyond the ratified scope fail as category errors; a smaller internalized component persists as out-of-scope groups tempering their claims to what courts will hear (roughly 80% structural, 20% internalized). Theater is low (0.22): the channel demonstrably fires (Reconstruction Amendments, 19th Amendment, 27th Amendment), so the process is functional, with a slow ceremonial accretion as amendments grow rarer. Accessibility collapse is moderate-low (0.40): the judicial route collapses completely under this framework, but state constitutions, ordinary legislation within scope, and the amendment channel itself remain live alternatives. Resistance is substantial (0.58): sustained movements press both the amendment route and repeated tests of the judicial boundary. The temporal series run on one shared grid (1788/1865/1920/1964/1992/2026) with every tracked metric authored at every point. The extractiveness curve dips as major expansions ratified (1865–1920) and turns mildly upward after 1992 — accumulation as the society's equality frontier moves faster than Article V can ratify, widening the gap between lived norms and codified scope; suppression rises monotonically as the enforcement machinery policing the boundary matured.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats should compute differently. From the amendment institutions' position the arrangement is the constitutional order working as designed: every expansion they have certified carries supermajority legitimacy no court could manufacture. From the out-of-scope groups' position the same gate is a lock to which they hold no key — they must assemble consent from the very constituencies the current scope protects, so their exclusion reproduces itself until their excluders move. The judiciary straddles: it administers the boundary daily and draws interpretive authority from being its exclusive reader, while absorbing the legitimacy cost of every denial that another framework would call injustice. The excluded seat (living-constitution advocates) registers maximal objection with zero procedural weight — the framework bars their methodology as a category, not case by case. The engine computes these divergences from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent scope members sit near the beneficiary end: the current distribution of protections persists until they consent otherwise, and they are the numerous side of every expansion vote. Constitutional amendment institutions also sit beneficiary-ward: the gate converts every inclusion movement into supermajority politics, handing them agenda leverage over claims they did not create. Out-of-scope groups sit near the full-target end: they bear the entire cost of exclusion, their exit options are trapped (emigration or subordination to state-level variation, both costly), and their identity-position as constitutional outsiders is not escapable by individual choice. Equality litigants are targets with somewhat better position — moderate resources, partial recovery through in-scope claims. The federal judiciary derives mid-range: it neither sets the gate's terms nor pays the exclusion costs, but collects interpretive authority while paying legitimacy costs. Spatial scope is national, which scales effective extraction modestly upward for the targets (verification of compliance across fifty state systems is harder) while leaving suppression unscaled — suppression is a raw structural property of the procedural foreclosure, not amplified by scope or power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how a polity changes its fundamental law legitimately, at a threshold high enough to block factional rewriting but low enough to permit correction — remains live, so no mandatrophy declaration is authored and the status-by-verdict pair (live x world_rearranges) raises no zombie flag. The classification guards against two symmetric mislabels. Calling the gate a snare ignores that the channel genuinely works for the excluded: the 13th, 14th, 15th, and 19th Amendments were carried by or for out-of-scope groups through the gate itself — the coordination function is not cover. Calling it a rope ignores the asymmetric incidence: the people whose status the gate decides are precisely the people who cannot trigger it, and the enforcement machinery actively forecloses their strongest alternative route. Tangled rope names both facts. The vestige-risk omega tracks the decay direction separately: if non-amendment channels absorb the gate's function while exclusion costs persist, the structure drifts piton-ward — but that is a monitored risk, not the current state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_delta,
    'This constraint is one reading of kernel equality_clause_scope (reading: progressive_textualist) — what would the sibling readings change structurally?',
    'Compile the sibling stories (restrictive_originalist, expansive_universalist) and compare victim sets, mechanism structure, and epsilon over the shared referent.',
    'restrictive_originalist deletes the expansion channel entirely — exclusion becomes the constraint''s content rather than its cost, the victim set empties, and epsilon re-indexes to insider coordination. expansive_universalist deletes the gate — application becomes immediate and universal, victims vanish, and epsilon approaches pure coordination. This reading''s tangled-rope profile exists only under the amendment-gated mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_delta, conceptual, 'Committer delta: how sibling readings of the equality-scope kernel restructure this constraint.').

omega_variable(
    disagreement_location,
    'Where exactly do the readings of kernel equality_clause_scope disagree?',
    'Structural comparison of the three readings'' axioms: the disagreement is located in the scope-setting mechanism (who may move the boundary — courts, supermajority electorates, or no one) and in the t0 referent population (whom the clause reached at enactment).',
    'Each location choice produces a different constraint with a different victim set and a different enforcement signature; mislocating the disagreement (e.g., treating it as a dispute about the principle''s goodness rather than its application mechanism) would collapse three constraints into one and destroy epsilon invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location, conceptual, 'Location of the inter-reading disagreement: mechanism and referent population, not the principle''s worth.').

omega_variable(
    amendment_gate_incidence,
    'Does the supermajority gate filter transient majorities (as designed — protecting minorities from factional rewriting) or entrench standing majorities against out-of-scope minorities (exclusion persisting until the excluded persuade their excluders)?',
    'Comparative analysis of amendment outcomes affecting out-of-scope groups against a counterfactual judicial-track timeline; amendment-difficulty literature across polities with different thresholds.',
    'If the gate primarily filters, measured costs to out-of-scope groups are coordination price and the profile shifts rope-ward; if it primarily entrenches, the costs are rent and the profile shifts snare-ward. Same structure, opposite verdicts — this omega carries the classification-relevant uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_gate_incidence, empirical, 'Whether the amendment threshold''s incidence is protective filtering or majoritarian entrenchment.').

omega_variable(
    era_deadline_counterfactual,
    'Did the Equal Rights Amendment''s failed ratification (passed Congress 1972, short of three-fourths by the extended 1982 deadline) show the gate blocking a just expansion or filtering an unstable one?',
    'Outcomes of revived-ratification litigation, archival analysis of the ratification coalitions, and comparison with jurisdictions that enacted comparable provisions statutorily.',
    'Calibrates the sign of the gate''s marginal effect on out-of-scope welfare: a blocked-justice reading raises effective extraction on expansion-seeking groups; a filtered-instability reading lowers it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(era_deadline_counterfactual, empirical, 'The ERA failure as a natural experiment on the gate''s filtering versus blocking behavior.').

omega_variable(
    threshold_vestige_risk,
    'As ordinary legislation and judicial doctrine absorb functions that amendments once performed, does the Article V gate persist as a live constraint or drift toward theatrical maintenance?',
    'Count scope-relevant changes accomplished by amendment versus non-amendment channels over trailing decades; track citation patterns of Article V doctrine.',
    'Rising theater_ratio with persistent exclusion costs is the classic decay signature: a gate that no longer governs while still excluding would push reclassification toward piton. Current data show the channel still fires (27th Amendment, 1992), keeping the ratio low.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_vestige_risk, conceptual, 'Vestigialization risk for the amendment gate as alternative channels absorb its function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__progressive_textualist, 1788, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eqscope_pt_tr_t1788, equality_clause_scope__progressive_textualist, theater_ratio, 1788, 0.08).
narrative_ontology:measurement_basis(eqscope_pt_tr_t1788, observed).
narrative_ontology:measurement(eqscope_pt_tr_t1865, equality_clause_scope__progressive_textualist, theater_ratio, 1865, 0.1).
narrative_ontology:measurement_basis(eqscope_pt_tr_t1865, observed).
narrative_ontology:measurement(eqscope_pt_tr_t1920, equality_clause_scope__progressive_textualist, theater_ratio, 1920, 0.12).
narrative_ontology:measurement_basis(eqscope_pt_tr_t1920, observed).
narrative_ontology:measurement(eqscope_pt_tr_t1964, equality_clause_scope__progressive_textualist, theater_ratio, 1964, 0.15).
narrative_ontology:measurement_basis(eqscope_pt_tr_t1964, observed).
narrative_ontology:measurement(eqscope_pt_tr_t1992, equality_clause_scope__progressive_textualist, theater_ratio, 1992, 0.19).
narrative_ontology:measurement_basis(eqscope_pt_tr_t1992, observed).
narrative_ontology:measurement(eqscope_pt_tr_t2026, equality_clause_scope__progressive_textualist, theater_ratio, 2026, 0.22).
narrative_ontology:measurement_basis(eqscope_pt_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(eqscope_pt_be_t1788, equality_clause_scope__progressive_textualist, base_extractiveness, 1788, 0.7).
narrative_ontology:measurement_basis(eqscope_pt_be_t1788, observed).
narrative_ontology:measurement(eqscope_pt_be_t1865, equality_clause_scope__progressive_textualist, base_extractiveness, 1865, 0.61).
narrative_ontology:measurement_basis(eqscope_pt_be_t1865, observed).
narrative_ontology:measurement(eqscope_pt_be_t1920, equality_clause_scope__progressive_textualist, base_extractiveness, 1920, 0.54).
narrative_ontology:measurement_basis(eqscope_pt_be_t1920, observed).
narrative_ontology:measurement(eqscope_pt_be_t1964, equality_clause_scope__progressive_textualist, base_extractiveness, 1964, 0.5).
narrative_ontology:measurement_basis(eqscope_pt_be_t1964, observed).
narrative_ontology:measurement(eqscope_pt_be_t1992, equality_clause_scope__progressive_textualist, base_extractiveness, 1992, 0.47).
narrative_ontology:measurement_basis(eqscope_pt_be_t1992, observed).
narrative_ontology:measurement(eqscope_pt_be_t2026, equality_clause_scope__progressive_textualist, base_extractiveness, 2026, 0.48).
narrative_ontology:measurement_basis(eqscope_pt_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(eqscope_pt_su_t1788, equality_clause_scope__progressive_textualist, suppression_requirement, 1788, 0.32).
narrative_ontology:measurement_basis(eqscope_pt_su_t1788, observed).
narrative_ontology:measurement(eqscope_pt_su_t1865, equality_clause_scope__progressive_textualist, suppression_requirement, 1865, 0.38).
narrative_ontology:measurement_basis(eqscope_pt_su_t1865, observed).
narrative_ontology:measurement(eqscope_pt_su_t1920, equality_clause_scope__progressive_textualist, suppression_requirement, 1920, 0.44).
narrative_ontology:measurement_basis(eqscope_pt_su_t1920, observed).
narrative_ontology:measurement(eqscope_pt_su_t1964, equality_clause_scope__progressive_textualist, suppression_requirement, 1964, 0.51).
narrative_ontology:measurement_basis(eqscope_pt_su_t1964, observed).
narrative_ontology:measurement(eqscope_pt_su_t1992, equality_clause_scope__progressive_textualist, suppression_requirement, 1992, 0.56).
narrative_ontology:measurement_basis(eqscope_pt_su_t1992, observed).
narrative_ontology:measurement(eqscope_pt_su_t2026, equality_clause_scope__progressive_textualist, suppression_requirement, 2026, 0.58).
narrative_ontology:measurement_basis(eqscope_pt_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__progressive_textualist, enforcement_mechanism).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__expansive_universalist).

% DUAL FORMULATION NOTE:
% The colloquial label 'the equality clause's scope' decomposes into three structurally distinct constraints — one per reading of kernel equality_clause_scope — each with its own epsilon, victim set, and mechanism: restrictive_originalist (no expansion channel; exclusion is content, victims empty out), progressive_textualist (this file; amendment-gated expansion; victims are the currently out-of-scope), expansive_universalist (no gate; immediate universal application; victims vanish). This reading is downstream of the ratified-text lineage both siblings cite: the originalist treats the pre-amendment text as exhaustive, the universalist treats the principle as outrunning any enactment, and this reading inherits the text's authority while conceding its incompleteness. Linked via affects_constraints per the constraint-family rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
