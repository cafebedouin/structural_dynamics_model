% ============================================================================
% CONSTRAINT STORY: family_estrangement_ratio
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_estrangement_ratio, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: family_estrangement_ratio
 *   human_readable: The 'Family is Forever' Dogma
 *   domain: social/family_dynamics
 *
 * SUMMARY:
 *   The 'Family is Forever' dogma is a social constraint that suppresses the
 *   option for individuals to make clean exits from harmful family
 *   relationships. The constraint operates across multiple institutional
 *   domains—religious teaching, therapeutic practice, cultural narratives,
 *   legal kinship structures—to position estrangement as moral failure,
 *   emotional immaturity, or psychological damage requiring repair. For abuse
 *   survivors and individuals in genuinely toxic family systems, this dogma
 *   creates a structural snare: the cultural/social costs of estrangement
 *   (shame, loss of identity, alienation from extended kin) are enforced
 *   through gossip, intervention attempts, religious condemnation, and
 *   therapeutic pressure toward reconciliation. The dogma has intensified
 *   over the past decade as social media has made estrangement more visible
 *   and as reconciliation narratives have proliferated in therapeutic and
 *   religious content, increasing the theater ratio (performative
 *   reconciliation work) while actual harm prevention has not improved. The
 *   constraint's extraction lies in the forced choice between maintaining
 *   contact with abusers or bearing the full social cost of rupture alone—no
 *   legitimate third path is culturally available.
 *
 * KEY AGENTS:
 *   - Estranged Individuals: Primary victims (powerless/trapped) — individuals who have ended family contact due to abuse, toxicity, or irreconcilable harm; bear full social cost of their decision
 *   - Abuse Survivors: Primary victims (powerless/trapped) — subset of estranged individuals dealing with documented abuse; face pressure to reconcile despite ongoing danger
 *   - Extended Family Members: Secondary victims (moderate/constrained) — feel obligated to maintain bonds, mediate conflict, and judge the estranged member's decision
 *   - Religious Institutions: Institutional enforcers (institutional/arbitrage) — churches and faith communities teach reconciliation as moral imperative; benefit from moral authority
 *   - Therapeutic Establishment: Institutional enforcers (institutional/arbitrage) — family therapy and psychology professions maintain reconciliation as default goal; profit from family counseling
 *   - Estrangement Support Networks: Organized challengers (organized/constrained) — support groups and trauma-informed practitioners building alternative legitimacy for estrangement; extract through positioning themselves as moral authority
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the dogma as inherent to kinship
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_estrangement_ratio, 0.58).
domain_priors:suppression_score(family_estrangement_ratio, 0.72).
domain_priors:theater_ratio(family_estrangement_ratio, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_estrangement_ratio, extractiveness, 0.58).
narrative_ontology:constraint_metric(family_estrangement_ratio, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(family_estrangement_ratio, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_estrangement_ratio, snare).
narrative_ontology:human_readable(family_estrangement_ratio, "The 'Family is Forever' Dogma").
narrative_ontology:topic_domain(family_estrangement_ratio, "social/family_dynamics").

% --- Structural relationships ---
narrative_ontology:constraint_victim(family_estrangement_ratio, estranged_individuals).
narrative_ontology:constraint_victim(family_estrangement_ratio, abuse_survivors).
narrative_ontology:constraint_victim(family_estrangement_ratio, relational_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ESTRANGED INDIVIDUAL (SNARE) — Trapped by social/family pressure to maintain contact despite harm. No culturally legitimated exit. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96. Maximum extraction: cannot exit without bearing severe social cost (shame, family rejection, alienation from extended kin, loss of cultural identity).
constraint_indexing:constraint_classification(family_estrangement_ratio, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EXTENDED FAMILY (SNARE) — Constrained by obligation to maintain kin bonds and pressure to 'fix' the estrangement. Faces internal role conflict and social scrutiny. d≈0.68, f(d)≈1.02, σ=0.9 → χ≈0.60. Moderate extraction: feels obligated to intervene, mediate, or judge the estranged member's decision.
constraint_indexing:constraint_classification(family_estrangement_ratio, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL ENFORCERS (ROPE) — Churches, therapists, cultural leaders, media narratives benefit from positioning 'family reconciliation' as the moral imperative. Arbitrage exit: can choose to enforce or relax the dogma without structural penalty. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.06. Net coordination benefit: the dogma provides institutional legitimacy and positions these actors as mediators.
constraint_indexing:constraint_classification(family_estrangement_ratio, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THERAPEUTIC ESTABLISHMENT (PITON) — Family therapy and psychology professions maintain reconciliation as default goal despite growing evidence that estrangement can be healthy. theater_ratio=0.68 (high performative content): therapeutic protocols emphasize 'working through' family conflict, reframing estrangement as failure of individual emotional regulation rather than rational boundary-setting. The therapeutic narrative persists through institutional inertia and lucrative family counseling markets, not because it consistently produces better outcomes.
constraint_indexing:constraint_classification(family_estrangement_ratio, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ESTRANGEMENT SUPPORT MOVEMENT (TANGLED ROPE) — Organized agents (support groups, advocacy networks, trauma-informed therapists) seek to legitimize estrangement as a valid boundary strategy. Both coordinate new norms (allowing estrangement) and extract through creating alternative institutional authority. d≈0.45, f(d)≈0.43, σ=1.2 → χ≈0.30. Moderate extraction: these organizations position themselves as moral gatekeepers of 'healthy estrangement' while criticizing traditional family enforcement.
constraint_indexing:constraint_classification(family_estrangement_ratio, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, kinship bonds are seen as natural/immutable: family relationships are inherent to human nature and cannot be authentically severed, only damaged or suppressed. This perspective risks naturalizing what is actually a contingent cultural norm. The structural data (ε=0.58, suppression=0.72, theater=0.68) contradicts mountain classification — the engine will flag this as a false summit, revealing that 'family is forever' is a culturally constructed dogma, not a law of nature.
constraint_indexing:constraint_classification(family_estrangement_ratio, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_estrangement_ratio_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(family_estrangement_ratio, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(family_estrangement_ratio, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_estrangement_ratio, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(family_estrangement_ratio, TR),
    TR >= 0.70.

:- end_tests(family_estrangement_ratio_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The dogma extracts from estranged individuals by forcing them to choose between contact with abusers and social isolation. The extraction is not maximal because some individuals do successfully exit and build alternative communities; the system lacks absolute enforcement mechanisms (legal, economic) beyond social pressure. The increasing measurement from 0.35 to 0.58 reflects growing institutional investment in reconciliation narratives and increasing visibility of estrangement in media/therapy discourse, which paradoxically increases suppression by making the choice more explicitly moralized. Suppression (0.72): High. Multiple reinforcing barriers prevent clean estrangement: (1) cultural/religious teaching that family bonds are sacred/permanent, (2) lack of institutional support for healthy estrangement (contrast with legal divorce), (3) therapeutic framing of estrangement as symptom of family dysfunction rather than rational response, (4) social media pressure and extended family intervention, (5) legal ambiguity about kinship dissolution (no formal social ritual). Theater ratio (0.68): High and increasing. Much of the institutional response to estrangement is performative: family therapy sessions that reframe the issue as individual emotional dysregulation, religious rituals emphasizing reconciliation without addressing abuse, social media performative shaming of estranged individuals as ungrateful/selfish. The rise from 0.42 to 0.68 reflects increased visibility and moralization of estrangement without corresponding increase in abuse prevention or structural support.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows a stark perspectival split between victims and enforcers. The estranged individual sees a snare: trapped by social cost. The extended family sees a snare too, but from a different angle: trapped by obligation. Institutional enforcers (churches, therapists) see a rope: they are solving the coordination problem of family harmony. But the analytical observer risks seeing a mountain—family bonds as natural law—when the constraint is actually a culturally constructed dogma enforced through institutional suppression. The estrangement support movement sees a tangled rope: they are creating new norms but also extracting through moral gatekeeping. The therapeutic establishment sees a piton: the reconciliation ritual persists through professional inertia despite low functional outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   Estranged individuals: Victims + trapped → d≈0.92, f(d)≈1.38. Maximum extraction: bear full social cost, cannot exit. Extended family: Victims + constrained → d≈0.68, f(d)≈1.02. Significant extraction: feel obligated to maintain bonds and judge the estranged member. Institutional enforcers (churches, therapists): Beneficiaries + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiaries: profit from reconciliation narratives. Estrangement support networks: Organized + constrained → d≈0.45, f(d)≈0.43. Moderate extraction: build alternative authority while criticizing traditional enforcement. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Risks naturalizing a contingent constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the snare classification (ε≈0.58) is correct: the constraint does exhibit high suppression (0.72) and primarily benefits institutional enforcers while harming estranged individuals. The false summit risk (mountain perspective) is a deliberate distortion used to defend the dogma—'family is forever' is positioned as natural law to avoid examining the institutional extraction. The estrangement support movement's tangled rope perspective reveals the constraint's true structure: there IS a coordination problem (how to maintain meaningful family relationships) but it has been solved through coercive suppression of exit options rather than through genuine coordination mechanisms. The therapeutic piton perspective is particularly clarifying: family therapy maintains the reconciliation ritual despite low evidence of benefit, and the ritual persists through professional investment in the market rather than through demonstrated effectiveness. The constraint is a snare masquerading as a mountain through institutional narrative power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_threshold_definition,
    'At what threshold of documented family harm (emotional abuse, physical violence, financial exploitation, sexual abuse) does estrangement become a rational boundary rather than emotional failure?',
    'Longitudinal outcome study comparing psychological health, financial stability, and social functioning of estranged vs reconciled abuse survivors; meta-analysis of family therapy success rates when abuse is present',
    'If threshold is low (documented abuse alone): estrangement is plainly rational, and the dogma becomes clearly extractive. If threshold is high or ambiguous: the dogma can claim most estrangements are premature, maintaining suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_threshold_definition, empirical, 'Harm threshold defining rational vs dysfunctional estrangement').

omega_variable(
    cultural_variance_authenticity,
    'Is the ''family is forever'' dogma a universal feature of human kinship or a specific cultural/historical construction?',
    'Comparative anthropology of kinship dissolution norms across cultures; historical analysis of when ''reconciliation at all costs'' became the Western norm (vs medieval/ancient practices of formal disownment, banishment)',
    'If universal: some version of the constraint may be natural law. If culturally constructed: the dogma is purely institutional, and suppression values are learned rather than inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_variance_authenticity, conceptual, 'Whether family permanence is universal or culturally constructed').

omega_variable(
    cost_of_false_reconciliation,
    'What are the aggregate social and psychological costs when estranged individuals are pressured into contact with abusers or toxic family members?',
    'Epidemiological study linking reconciliation pressure to rates of depression, PTSD, substance abuse, and suicide in estranged populations; cost-benefit analysis comparing healthcare/social service costs of forced reconciliation vs supported estrangement',
    'If costs are high: the dogma''s suppression directly harms victims, strengthening snare classification. If costs are low: the constraint might be better modeled as rope (coordination problem) rather than snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_of_false_reconciliation, empirical, 'Health and social costs of forced reconciliation').

omega_variable(
    institutional_incentive_alignment,
    'Do therapeutic, religious, and cultural institutions that enforce the dogma profit or gain authority from maintaining the suppression of estrangement as a valid option?',
    'Economic analysis of family therapy market size and growth; content analysis of religious texts and contemporary sermons positioning reconciliation as moral imperative; institutional profit motive studies',
    'If institutions profit: the rope perspective (institutional enforcers) is revealed as a beneficiary rather than neutral arbiter, strengthening snare classification. If institutions are indifferent: constraint might be explained by pure cultural transmission rather than active extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_incentive_alignment, empirical, 'Whether institutions profit from enforcing the reconciliation dogma').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_estrangement_ratio, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fam_est_tr_t0, family_estrangement_ratio, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fam_est_tr_t5, family_estrangement_ratio, theater_ratio, 5, 0.55).
narrative_ontology:measurement(fam_est_tr_t10, family_estrangement_ratio, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(fam_est_be_t0, family_estrangement_ratio, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fam_est_be_t5, family_estrangement_ratio, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(fam_est_be_t10, family_estrangement_ratio, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_estrangement_ratio, enforcement_mechanism).
narrative_ontology:affects_constraint(family_estrangement_ratio, reproductive_coercion_social_pressure).
narrative_ontology:affects_constraint(family_estrangement_ratio, domestic_abuse_reporting_suppression).

% DUAL FORMULATION NOTE:
% The 'Family is Forever' dogma is downstream of broader kin-obligation norms but represents a distinct structural constraint focused specifically on suppressing estrangement as a valid option. Related constraints (reproductive coercion, domestic abuse reporting suppression) operate within the same institutional ecosystem and are reinforced by the same narrative of family permanence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
