% ============================================================================
% CONSTRAINT STORY: creed_381_pneumatology__ecumenical_reunion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creed_381_pneumatology__ecumenical_reunion_reading, []).

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
 *   constraint_id: creed_381_pneumatology__ecumenical_reunion_reading
 *   human_readable: Nicene-Constantinopolitan Pneumatology — Ecumenical Reunion Arrangement (Mutual Recognition of Regional Expressions)
 *   domain: religious/ecclesiastical-authority/commitment-systems
 *
 * SUMMARY:
 *   Within the post-conciliar ecumenical process between Rome and the
 *   Orthodox churches, an arrangement has taken shape under which the
 *   Spirit's procession 'from the Father and the Son' (the Western clause)
 *   and 'from the Father alone' (the original Greek text of 381) are each
 *   treated as a legitimate regional theological expression of one shared
 *   faith, held within a single envisioned communion. Bilateral recognition —
 *   each communion formally acknowledging the other's expression — replaces
 *   the historical pattern in which one side inserted or demanded the clause
 *   unilaterally. The arrangement is embodied in agreed dialogue statements,
 *   the 1995 Vatican clarification distinguishing the dogma from its
 *   theological articulations, the printing of the creed without the added
 *   clause in multilateral liturgical texts, and joint episcopal recitations
 *   of the Greek form. Its participants frame it as transitional: a framework
 *   that holds the communions in contact until fuller agreement renders the
 *   dual expression unnecessary. The costs it imposes are modest and
 *   non-material — the suspension of confessional exclusivity claims and the
 *   perpetual labor of dialogue — and its enforcement is maintenance rather
 *   than coercion: commissions, agreed texts, and reciprocal liturgical
 *   practice. KEY AGENTS (by structural relationship): roman_apostolic_see
 *   and orthodox_autocephalous_synods — co-agenda-setters
 *   (institutional/mobile) who grant recognition and could withdraw it;
 *   ecumenical_theological_commissions — administering beneficiary
 *   (institutional/constrained) whose mandate depends on the process
 *   continuing; eastern_catholic_communities — beneficiary
 *   (organized/identity_locked) living between the traditions;
 *   confessional_traditionalists — principal cost-bearers
 *   (organized/identity_locked) whose exclusivity claims the arrangement
 *   suspends; ordinary_clergy_and_laity — diffuse beneficiaries
 *   (moderate/constrained); protestant_faith_order_bodies — external
 *   beneficiaries (institutional/mobile); doctrinal_historians — analytical
 *   observers. Reading-indexed contestation of the underlying creedal kernel
 *   is recorded in the omega variables and kernel_context, not folded into
 *   this constraint's classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creed_381_pneumatology__ecumenical_reunion_reading, 0.31).
domain_priors:suppression_score(creed_381_pneumatology__ecumenical_reunion_reading, 0.3).
domain_priors:theater_ratio(creed_381_pneumatology__ecumenical_reunion_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__ecumenical_reunion_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(creed_381_pneumatology__ecumenical_reunion_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__ecumenical_reunion_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(creed_381_pneumatology__ecumenical_reunion_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creed_381_pneumatology__ecumenical_reunion_reading, scaffold).
narrative_ontology:human_readable(creed_381_pneumatology__ecumenical_reunion_reading, "Nicene-Constantinopolitan Pneumatology — Ecumenical Reunion Arrangement (Mutual Recognition of Regional Expressions)").
narrative_ontology:topic_domain(creed_381_pneumatology__ecumenical_reunion_reading, "religious/ecclesiastical-authority/commitment-systems").

domain_priors:requires_active_enforcement(creed_381_pneumatology__ecumenical_reunion_reading).
narrative_ontology:has_sunset_clause(creed_381_pneumatology__ecumenical_reunion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(creed_381_pneumatology__ecumenical_reunion_reading, '397b347f-2a4b-4f21-a200-ef2f6f7a40d8').
narrative_ontology:cs_kernel_codification('397b347f-2a4b-4f21-a200-ef2f6f7a40d8', fixed_text).
narrative_ontology:cs_authority_grounding('397b347f-2a4b-4f21-a200-ef2f6f7a40d8', lineage).
narrative_ontology:cs_interpretation_layer_present('397b347f-2a4b-4f21-a200-ef2f6f7a40d8').
narrative_ontology:cs_reading_relation('397b347f-2a4b-4f21-a200-ef2f6f7a40d8', creed_381_pneumatology__filioque_reading, influences).
narrative_ontology:cs_reading_relation('397b347f-2a4b-4f21-a200-ef2f6f7a40d8', creed_381_pneumatology__monoprocession_reading, influences).
narrative_ontology:cs_axiom('397b347f-2a4b-4f21-a200-ef2f6f7a40d8', foundational, regional_expression_legitimacy).
narrative_ontology:cs_axiom_status(regional_expression_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('397b347f-2a4b-4f21-a200-ef2f6f7a40d8', regional_expression_legitimacy, conventional).
narrative_ontology:cs_axiom('397b347f-2a4b-4f21-a200-ef2f6f7a40d8', foundational, consent_based_doctrinal_authority).
narrative_ontology:cs_axiom_status(consent_based_doctrinal_authority, holdable).
narrative_ontology:cs_axiom_grounding('397b347f-2a4b-4f21-a200-ef2f6f7a40d8', consent_based_doctrinal_authority, instrumental).
narrative_ontology:cs_reference_frame('397b347f-2a4b-4f21-a200-ef2f6f7a40d8', common_creed_regional_pneumatology).
narrative_ontology:cs_drift_state('397b347f-2a4b-4f21-a200-ef2f6f7a40d8', contemporary_post_clarification_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('397b347f-2a4b-4f21-a200-ef2f6f7a40d8', '').
narrative_ontology:cs_kernel_id(creed_381_pneumatology__ecumenical_reunion_reading, creed_381_pneumatology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__ecumenical_reunion_reading, ecumenical_theological_commissions).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__ecumenical_reunion_reading, eastern_catholic_communities).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__ecumenical_reunion_reading, ordinary_clergy_and_laity).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__ecumenical_reunion_reading, protestant_faith_order_bodies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__ecumenical_reunion_reading, roman_apostolic_see).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__ecumenical_reunion_reading, orthodox_autocephalous_synods).
narrative_ontology:constraint_victim(creed_381_pneumatology__ecumenical_reunion_reading, confessional_traditionalists).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__ecumenical_reunion_reading, legitimate_theological_diversity_principle).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__ecumenical_reunion_reading, consent_based_amendment_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Grants recognition that the Eastern expression of the Spirit's procession is a legitimate articulation within its own tradition, while retaining the added clause in its own liturgical use; signs joint declarations and receives the prospect of restored communion. It can withdraw recognition and revert to requiring the clause universally at any time, having done so for centuries before the dialogue began.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__ecumenical_reunion_reading, roman_apostolic_see, agenda_setter,
    institutional, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__ecumenical_reunion_reading, roman_apostolic_see, beneficiary).

% Grant recognition that the Western expression reflects a permissible theological articulation, while keeping the creed's original Greek form as the common text; they receive the prospect of unity without surrendering the 381 wording. Any synod can withdraw from the arrangement by reaffirming the creed's inviolability, and several periodically do so in protest.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__ecumenical_reunion_reading, orthodox_autocephalous_synods, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__ecumenical_reunion_reading, orthodox_autocephalous_synods, beneficiary).

% Draft the agreed statements, organize plenaries, and monitor how mutual recognition is implemented; their mandate, staffing, and convening power depend on the process continuing. Their exit is dissolution — if the parties completed union or abandoned it, the commissions as constituted would have no reason to exist.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__ecumenical_reunion_reading, ecumenical_theological_commissions, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__ecumenical_reunion_reading, ecumenical_theological_commissions, agenda_setter).

% Communities in full communion with Rome but worshipping in the Byzantine rite; mutual recognition eases the strain of belonging to both worlds and validates their inherited usage. Leaving either side would dissolve the bridging identity that constitutes them, so they accommodate whatever the two communions agree.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__ecumenical_reunion_reading, eastern_catholic_communities, beneficiary,
    organized, generational, identity_locked, continental).

% Monastic and lay movements in both communions who hold that only one procession formula is true and that acknowledging the other betrays the creed; the arrangement costs them the official standing of their exclusivity claim and requires them to tolerate what they judge to be error. Their confessional identity forbids the accommodation the arrangement asks of them, and periodic protest campaigns are their principal recourse.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__ecumenical_reunion_reading, confessional_traditionalists, payer,
    organized, biographical, identity_locked, continental).

% Parish priests and congregations receive the practical fruits — reciprocal liturgical hospitality, joint declarations read from pulpits, diminished polemical literature — while mostly continuing inherited usage unchanged; their exposure to the arrangement is indirect and their ability to shape it limited.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__ecumenical_reunion_reading, ordinary_clergy_and_laity, beneficiary,
    moderate, immediate, constrained, global).

% Multilateral ecumenical bodies that cite the bilateral recognition model as a method for their own divided constituencies; they gain a working precedent and contribute study documents, and could redirect their attention elsewhere at little cost.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__ecumenical_reunion_reading, protestant_faith_order_bodies, beneficiary,
    institutional, generational, mobile, global).

% Scholars of the credal controversy who trace how the clause entered Western usage and how the two traditions articulate the procession; they publish assessments of whether the mutual-recognition account is historically accurate and are bound by no confessional stake in the outcome.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__ecumenical_reunion_reading, doctrinal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(creed_381_pneumatology__ecumenical_reunion_reading, ecumenical_theological_commissions).
narrative_ontology:fixing_cost_class(creed_381_pneumatology__ecumenical_reunion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common creedal reference (the Greek text of 381) and a recognition protocol under which two regional pneumatological expressions can coexist within one projected communion, so that doctrinal uniformity on the procession question ceases to be a precondition for unity.
% TRANSFER_FUNCTION: Moves recognition and legitimacy bilaterally between the communions — each grants standing to the other's expression — and relocates the disputed clause from universal-confession status to regional-expression status; secondarily it moves attention, personnel, and resources into the dialogue apparatus.
% ABSENT_VOICES: Radical traditionalists on both sides (Old Calendarist jurisdictions, sedevacantist circles) decline participation and would object that pluralism licenses error; the lay faithful are thinly represented in the commissions; the Oriental Orthodox churches, whose parallel Christological settlement is often cited as precedent, are not party to this arrangement.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, joint declarations and shared creedal recitations would cease, the 1995 clarification would lose its operative framework, eastern Catholic communities would lose their bridging legitimacy, the dialogue commissions would dissolve for want of a mandate, and bilateral relations would revert to the pre-dialogue posture in which each communion demands the other's formula as a communion precondition.
% FOUNDING_PROBLEM: The millennium-old division between the communions, crystallized by the unilateral insertion of the Filioque into the creed and the ensuing dispute over both the procession doctrine itself and the authority to amend the common creed.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the World Council of Churches' Faith and Order Commission (a multilateral body not party to the bilateral arrangement) attests the division's persistence in its convergence texts; academic historians of doctrine, working outside both communions' magisteria, attest both the controversy's history and the novelty of the bilateral-recognition approach; the continued absence of intercommunion between the two sides corroborates that the founding problem remains unsolved.
narrative_ontology:disappearance_verdict(creed_381_pneumatology__ecumenical_reunion_reading, world_rearranges).
narrative_ontology:founding_problem_status(creed_381_pneumatology__ecumenical_reunion_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(creed_381_pneumatology__ecumenical_reunion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(creed_381_pneumatology__ecumenical_reunion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(creed_381_pneumatology__ecumenical_reunion_reading, 0.31, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creed_381_pneumatology__ecumenical_reunion_reading_tests).
:- end_tests(creed_381_pneumatology__ecumenical_reunion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.31 at interval end): the arrangement collects no material rents; what it takes is the official standing of confessional exclusivity claims and the ongoing resource commitment the dialogue apparatus consumes. Suppression is low (0.30): nothing coerces dissenters, though managing traditionalist protest after contentious agreed statements (the mid-1990s reception crisis is visible as the steepest single step in the series) requires real administrative attention. Theater_ratio (0.46) is the story's sharpest signal: joint declarations have proliferated faster than structural convergence, and ceremonial creedal recitations increasingly perform unity that the rank-and-file have not yet received — a Goodhart-drift trajectory approaching, but not crossing, the 0.5 proxy-substitution threshold. Accessibility_collapse is low (0.30): alternatives remain fully available to every party — either communion can revert to its unilateral position, and continued separation remains the standing default — which is what distinguishes this construct from anything resembling natural law. Resistance is moderate (0.45): sustained, organized, non-violent pushback from confessional traditionalists in both communions, sufficient to slow implementation but not to stop the process. The three temporal series share one grid (decennial points 1980-2025) so no metric is sampled against another's end-state. Suppression_requirement is tracked deliberately: the arrangement's maintenance machinery (commissions, agreed texts, reception management) matured and hardened through the 1990s and then plateaued — an enforcement-capacity trajectory, not merely a shift in extraction. The claim (scaffold) and the metrics are authored independently: the claim rests on the arrangement's transitional structure and declared sunset orientation; the metrics describe its measured operation, including the theater accumulation that threatens the claim.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the two co-agenda-setter seats (Rome, the autocephalous synods), the arrangement is an achievement they built and can leave at will — mobile exit makes it a chosen framework, experienced as coordination. From the administering seat (the commissions), the arrangement is a vocation and a mandate whose continuity is their institutional existence — constrained exit colors everything they author about it. From the traditionalist seat, the same structure operates as the suspension of confessional truth by administrative consensus — identity_locked exit means they cannot take the arrangement's bargain without betraying what they are, so they experience pure cost. From the eastern Catholic seat, it is relief — validation of a bridging existence they cannot exit. The engine computes these divergent per-seat classifications from the structural data (power, exit, role); this story's single claimed type does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The four declared beneficiary groups derive low directionality (subsidized seats): the commissions are fed by the process, the eastern Catholic communities and ordinary faithful receive its fruits, and the Protestant bodies harvest a reusable method at negligible cost. The co-agenda-setters sit near symmetric: each gives recognition and receives recognition, and both retain full exit. The traditionalists are the structural targets — they bear the arrangement's actual costs (suspended exclusivity, managed dissent) — and their identity_locked exit pushes them toward the full-target end of the directionality scale despite the absence of a declared victim set; that absence is itself an authored claim under this reading's own lights (the arrangement withholds endorsement, not liberty), and it is guarded by the traditionalist_cost_status omega rather than assumed. No directionality overrides were needed: role, exit, and power atoms separate the seats cleanly, and the override mechanism's power-atom granularity would misfire across same-atom stakeholders (organized beneficiaries and organized cost-bearers share an atom).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live — the schism persists and the procession question is unresolved at the level of full agreement — so this is not a resolved-mandatrophy case, and the status-by-verdict pair (live x world_rearranges) raises no zombie flag. But the scaffold carries its characteristic pathology in embryo, and the receipt surface names it: gain_flow lands on the ecumenical_theological_commissions, the seat whose institutional existence depends on the transition NOT completing, and fixing_cost is prohibitive — completing the union the arrangement points toward exceeds any party's capacity, so the administrator cannot cash out the sunset even if it wished to. The rising theater_ratio (0.15 to 0.46 over the interval) is the early-warning signature: declaration volume substituting for convergence. If the sunset condition never arrives — if dual expression normalizes as a permanent steady state — the transitional justification fails and the arrangement drifts toward piton: maintained theatrically by the very apparatus that profits from its incompleteness, bearing diffuse costs on traditionalists whom no one is hurt enough to appease. The sunset_condition_specification and apparatus_self_perpetuation omegas are the monitoring instruments for exactly this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates the ecumenical_reunion_reading of the creed_381_pneumatology kernel; the sibling readings (filioque_reading, monoprocession_reading) would instantiate structurally different constraints over the same creedal text — where exactly does the disagreement bind?',
    'Comparative authoring of the sibling stories: filioque_reading yields a constraint with the magisterium as agenda-setting seat and high enforcement over a universal-clause arrangement; monoprocession_reading yields a constraint structured around conciliar-consent rules and breach-framing. The disagreement binds at two structural points: whether the creed''s transmitted text is amendable at all, and which seat holds authority to settle pneumatology (magisterium, ecumenical consent, or bilateral recognition).',
    'If a sibling reading prevails, this arrangement''s beneficiary set collapses (the pluralism license is revoked), the sunset condition changes meaning entirely, and epsilon recomputes over a different standing arrangement with a different victim structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Reading-indexed identity of the constraint within the contested creed_381_pneumatology kernel.').

omega_variable(
    sunset_condition_specification,
    'What observable event constitutes this arrangement''s sunset — restoration of full communion, a jointly agreed pneumatological confession, or normalization of dual expression as a permanent steady state?',
    'Watch for a joint declaration of sacramental communion between the sees, or a formal abandonment of the unity goal by either party; either endpoint resolves the ambiguity.',
    'If the parties normalize coexistence as permanent, the arrangement''s transitional justification fails and it reclassifies toward piton (or toward rope if the steady state proves benign); if a completion event arrives, the scaffold resolves as designed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_condition_specification, empirical, 'Whether the arrangement retains a live transition endpoint or the sunset is drifting into fiction.').

omega_variable(
    apparatus_self_perpetuation,
    'Does the dialogue apparatus accrue sufficient institutional benefit from the transition remaining incomplete that its maintenance effort subtly favors perpetuation over resolution?',
    'Track the composition of commission output over time: the ratio of procedural and reaffirmation documents to substantive convergence documents, alongside budget and staffing trajectories.',
    'An affirmative finding supports the rising theater_ratio trajectory and eventual piton drift; a negative finding supports the arrangement''s genuine transitional function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apparatus_self_perpetuation, empirical, 'Mandatrophy risk seated in the administering body of the transition.').

omega_variable(
    reception_depth,
    'Is bilateral recognition received below the hierarchical level — do clergy and laity of both communions actually treat the rival expression as legitimate, or does the arrangement subsist as elite consensus among negotiators?',
    'Survey and liturgical-practice data: incidence of cross-communion sacramental sharing, seminary teaching materials on the procession question, parish-level adoption of the creed''s Greek form.',
    'Shallow reception means the coordination function is largely nominal (higher theater, weaker rope-component beneath the scaffold); deep reception strengthens the genuine-coordination reading of the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reception_depth, empirical, 'Depth of the consensus beneath the negotiating tables.').

omega_variable(
    traditionalist_cost_status,
    'Do confessional traditionalists constitute an unnamed victim set bearing asymmetric costs (suspended exclusivity, marginalization within their own communions), or are they free dissenters whose position loses only official endorsement?',
    'Examine sanction records across both communions: whether traditionalist clergy face career, canonical, or liturgical penalties for rejecting the arrangement, versus mere loss of majority standing.',
    'If sanctions are systematic, a victim set must be declared and the classification shifts toward tangled_rope; if not, the no-victim consensus-model reading of this arrangement stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditionalist_cost_status, empirical, 'Whether the consensus model conceals an unnamed victim set.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creed_381_pneumatology__ecumenical_reunion_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cree_tr_t1980, creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement_basis(cree_tr_t1980, observed).
narrative_ontology:measurement(cree_tr_t1985, creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement_basis(cree_tr_t1985, observed).
narrative_ontology:measurement(cree_tr_t1990, creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement_basis(cree_tr_t1990, observed).
narrative_ontology:measurement(cree_tr_t1995, creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 1995, 0.28).
narrative_ontology:measurement_basis(cree_tr_t1995, observed).
narrative_ontology:measurement(cree_tr_t2000, creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement_basis(cree_tr_t2000, observed).
narrative_ontology:measurement(cree_tr_t2005, creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 2005, 0.33).
narrative_ontology:measurement_basis(cree_tr_t2005, observed).
narrative_ontology:measurement(cree_tr_t2010, creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 2010, 0.36).
narrative_ontology:measurement_basis(cree_tr_t2010, observed).
narrative_ontology:measurement(cree_tr_t2015, creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement_basis(cree_tr_t2015, observed).
narrative_ontology:measurement(cree_tr_t2020, creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 2020, 0.44).
narrative_ontology:measurement_basis(cree_tr_t2020, observed).
narrative_ontology:measurement(cree_tr_t2025, creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 2025, 0.46).
narrative_ontology:measurement_basis(cree_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(cree_be_t1980, creed_381_pneumatology__ecumenical_reunion_reading, base_extractiveness, 1980, 0.18).
narrative_ontology:measurement_basis(cree_be_t1980, observed).
narrative_ontology:measurement(cree_be_t1985, creed_381_pneumatology__ecumenical_reunion_reading, base_extractiveness, 1985, 0.2).
narrative_ontology:measurement_basis(cree_be_t1985, observed).
narrative_ontology:measurement(cree_be_t1990, creed_381_pneumatology__ecumenical_reunion_reading, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement_basis(cree_be_t1990, observed).
narrative_ontology:measurement(cree_be_t1995, creed_381_pneumatology__ecumenical_reunion_reading, base_extractiveness, 1995, 0.24).
narrative_ontology:measurement_basis(cree_be_t1995, observed).
narrative_ontology:measurement(cree_be_t2000, creed_381_pneumatology__ecumenical_reunion_reading, base_extractiveness, 2000, 0.26).
narrative_ontology:measurement_basis(cree_be_t2000, observed).
narrative_ontology:measurement(cree_be_t2005, creed_381_pneumatology__ecumenical_reunion_reading, base_extractiveness, 2005, 0.27).
narrative_ontology:measurement_basis(cree_be_t2005, observed).
narrative_ontology:measurement(cree_be_t2010, creed_381_pneumatology__ecumenical_reunion_reading, base_extractiveness, 2010, 0.28).
narrative_ontology:measurement_basis(cree_be_t2010, observed).
narrative_ontology:measurement(cree_be_t2015, creed_381_pneumatology__ecumenical_reunion_reading, base_extractiveness, 2015, 0.29).
narrative_ontology:measurement_basis(cree_be_t2015, observed).
narrative_ontology:measurement(cree_be_t2020, creed_381_pneumatology__ecumenical_reunion_reading, base_extractiveness, 2020, 0.3).
narrative_ontology:measurement_basis(cree_be_t2020, observed).
narrative_ontology:measurement(cree_be_t2025, creed_381_pneumatology__ecumenical_reunion_reading, base_extractiveness, 2025, 0.31).
narrative_ontology:measurement_basis(cree_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(cree_su_t1980, creed_381_pneumatology__ecumenical_reunion_reading, suppression_requirement, 1980, 0.12).
narrative_ontology:measurement_basis(cree_su_t1980, observed).
narrative_ontology:measurement(cree_su_t1985, creed_381_pneumatology__ecumenical_reunion_reading, suppression_requirement, 1985, 0.15).
narrative_ontology:measurement_basis(cree_su_t1985, observed).
narrative_ontology:measurement(cree_su_t1990, creed_381_pneumatology__ecumenical_reunion_reading, suppression_requirement, 1990, 0.18).
narrative_ontology:measurement_basis(cree_su_t1990, observed).
narrative_ontology:measurement(cree_su_t1995, creed_381_pneumatology__ecumenical_reunion_reading, suppression_requirement, 1995, 0.24).
narrative_ontology:measurement_basis(cree_su_t1995, observed).
narrative_ontology:measurement(cree_su_t2000, creed_381_pneumatology__ecumenical_reunion_reading, suppression_requirement, 2000, 0.26).
narrative_ontology:measurement_basis(cree_su_t2000, observed).
narrative_ontology:measurement(cree_su_t2005, creed_381_pneumatology__ecumenical_reunion_reading, suppression_requirement, 2005, 0.27).
narrative_ontology:measurement_basis(cree_su_t2005, observed).
narrative_ontology:measurement(cree_su_t2010, creed_381_pneumatology__ecumenical_reunion_reading, suppression_requirement, 2010, 0.28).
narrative_ontology:measurement_basis(cree_su_t2010, observed).
narrative_ontology:measurement(cree_su_t2015, creed_381_pneumatology__ecumenical_reunion_reading, suppression_requirement, 2015, 0.29).
narrative_ontology:measurement_basis(cree_su_t2015, observed).
narrative_ontology:measurement(cree_su_t2020, creed_381_pneumatology__ecumenical_reunion_reading, suppression_requirement, 2020, 0.3).
narrative_ontology:measurement_basis(cree_su_t2020, observed).
narrative_ontology:measurement(cree_su_t2025, creed_381_pneumatology__ecumenical_reunion_reading, suppression_requirement, 2025, 0.3).
narrative_ontology:measurement_basis(cree_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creed_381_pneumatology__ecumenical_reunion_reading, information_standard).
narrative_ontology:affects_constraint(creed_381_pneumatology__ecumenical_reunion_reading, creed_381_pneumatology__filioque_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__ecumenical_reunion_reading, creed_381_pneumatology__monoprocession_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'the Filioque problem' per the epsilon-invariance principle. The single natural-language controversy covers three structurally distinct arrangements, each with its own stable epsilon and its own beneficiary/victim structure: the filioque_reading authors epsilon over the magisterial-imposition arrangement (high extraction, enforced universality of the clause); the monoprocession_reading authors epsilon over the unilateral-amendment/breach arrangement (high extraction from the consenting-churches norm); this story authors epsilon (0.31) over the mutual-recognition arrangement alone. The upstream historical claims (how the clause entered Western usage; what 381 actually transmitted) feed all three readings as shared evidentiary ground, which is why the family is linked rather than independent. Each member must be evaluated at its own referent; averaging epsilon across readings would fabricate a constraint none of the parties holds.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
