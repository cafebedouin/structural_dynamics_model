% ============================================================================
% CONSTRAINT STORY: voluntary_presence_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_voluntary_presence_constraint, []).

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
 *   constraint_id: voluntary_presence_constraint
 *   human_readable: Voluntary Presence Constraint in Intimate Relationships
 *   domain: philosophy_of_mind/social_psychology/relationship_ethics
 *
 * SUMMARY:
 *   The voluntary presence constraint operates at the philosophical
 *   intersection of authentic choice and structural compulsion in intimate
 *   relationships. When exit from a relationship carries catastrophic cost —
 *   economic ruin (loss of shared assets, income dependency), social
 *   isolation (dismantled support networks, custody loss), identity
 *   dissolution (self-concept constituted through the relationship), or
 *   family fragmentation — continued presence becomes structurally ambiguous.
 *   The partner may be staying because they choose to (authentic love,
 *   genuine coordination) or because they cannot leave (trapped by exit-cost
 *   architecture, material dependence, identity fusion). The constraint's
 *   core extraction mechanism is the asymmetric engineering of exit costs:
 *   one partner benefits from the other's continued presence while the cost
 *   of departure is made catastrophic, creating a bind where presence appears
 *   voluntary but is structurally compelled. This constraint demonstrates how
 *   authentic-seeming relationships can embed extractive mechanisms, how
 *   control can operate through suppression of alternatives rather than overt
 *   coercion, and how institutional arrangements (marriage law, economic
 *   coupling, social norms, childcare architecture) engineer the exit-cost
 *   barriers that make departure unthinkable. The theater ratio (0.55)
 *   reflects the gap between relational narrative (presence as love, choice,
 *   commitment) and structural reality (presence constrained by engineered
 *   barriers). The rising suppression and extractiveness over the measurement
 *   interval model the progressive tightening of the constraint as financial
 *   coupling deepens, children are born, and the partner's identity becomes
 *   fused with the relationship.
 *
 * KEY AGENTS:
 *   - Structurally Dependent Partner: Primary victim (powerless/trapped) — bears full cost of suppression through economic dependency, social isolation, identity fusion, and custody barriers. Presence is compelled by catastrophic exit costs.
 *   - Controlling Partner: Primary beneficiary (institutional/arbitrage) — benefits from continued presence through emotional labor, financial consolidation, care provision, and identity regulation. Extracts compliance through suppression architecture while experiencing the constraint as coordination.
 *   - Partner with Partial Exit Capacity: Secondary agent (moderate/constrained) — faces high but not insurmountable costs; also receives genuine relational benefits. Represents mixed coordination-extraction experience.
 *   - Relationship Support Infrastructure: Organized agents (organized/constrained) — legal frameworks, social programs, cultural norms building alternative pathways that reduce exit-cost barriers and enable genuine choice. Have agency and sunset perspective.
 *   - Marriage Institution: Institutional actor (institutional/arbitrage) — maintains the formal structure through cultural inertia and legal embedding despite degraded original coordination functions. Performs degraded ritual rather than core function (Piton).
 *   - Identity-Locked Analyst: Professional embedded in relationship institutions (analytical/identity_locked) — sees the structural extraction but cannot fully exit the interpretive frame because their identity is constituted through the institution. Instantiates the oracle gap.
 *   - Relationship Authenticity: Abstract victim (powerless/trapped) — the quality of genuine care and mutual choice is corroded by the suppression mechanism. Cannot organize or advocate for itself.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(voluntary_presence_constraint, 0.58).
domain_priors:suppression_score(voluntary_presence_constraint, 0.68).
domain_priors:theater_ratio(voluntary_presence_constraint, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(voluntary_presence_constraint, extractiveness, 0.58).
narrative_ontology:constraint_metric(voluntary_presence_constraint, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(voluntary_presence_constraint, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(voluntary_presence_constraint, tangled_rope).
narrative_ontology:human_readable(voluntary_presence_constraint, "Voluntary Presence Constraint in Intimate Relationships").
narrative_ontology:topic_domain(voluntary_presence_constraint, "philosophy_of_mind/social_psychology/relationship_ethics").

domain_priors:requires_active_enforcement(voluntary_presence_constraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(voluntary_presence_constraint, controlling_partner).
narrative_ontology:constraint_victim(voluntary_presence_constraint, structurally_dependent_partner).
narrative_ontology:constraint_victim(voluntary_presence_constraint, relationship_authenticity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STRUCTURALLY DEPENDENT PARTNER (SNARE) — Faces catastrophic exit costs: economic ruin (loss of shared assets, income dependency), social isolation (dismantled support network), identity dissolution (self-concept constituted through the relationship), child custody loss (legal and financial barriers). No meaningful exit option. Maximum experienced extraction — presence is compelled by material barriers, not authentic choice. The partner bears the full cost of suppression (inability to leave) and derives no net benefit from the constraint.
constraint_indexing:constraint_classification(voluntary_presence_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CONTROLLING PARTNER (ROPE) — Experiences the constraint as coordination: maintaining the relationship requires managing the other partner's presence through exit-cost architecture (financial control, isolation, emotional manipulation). Net beneficiary — extracts compliance through suppression without perceiving themselves as extractive. The constraint solves their coordination problem: keeping the partner present. Minimal suppression from their perspective; maximum benefit.
constraint_indexing:constraint_classification(voluntary_presence_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: PARTNER WITH PARTIAL EXIT CAPACITY (TANGLED ROPE) — Faces high but not insurmountable exit costs (career interruption, housing loss, financial penalty, social damage). Also receives genuine relational benefits: emotional support, shared projects, intimacy infrastructure. Mixed experience — significant extraction but not total, some coordination function, constrained but not trapped. Represents the modal case where relationships combine authentic coordination with embedded extraction.
constraint_indexing:constraint_classification(voluntary_presence_constraint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: RELATIONSHIP SUPPORT INFRASTRUCTURE (SCAFFOLD) — Legal frameworks (no-fault divorce, spousal support, child custody protections, domestic violence statutes, asset division), social programs (emergency housing, job retraining, childcare support), and cultural norms (relationship counseling, safe-exit resources) are building alternative pathways that reduce catastrophic exit costs. These structured interventions have sunset logic — as they mature and become accessible, the voluntary presence constraint's suppression mechanism loses force. Organized agents (legal aid, shelters, counselors) have agency and see an exit path.
constraint_indexing:constraint_classification(voluntary_presence_constraint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: MARRIAGE INSTITUTION (PITON) — Marriage as a formal institution persists through cultural/legal inertia despite degraded primary function. Historically coordinated economic survival, property transfer, child legitimacy, and social belonging. Modern marriage has attenuated these functions (economic independence possible, property law reformed, child legitimacy decoupled from marital status, social belonging available elsewhere), yet the institutional form persists with theater — wedding rituals, legal ceremonies, cultural expectation narratives. The institution sees its own function as degraded but maintains the form because alternatives haven't fully displaced it.
constraint_indexing:constraint_classification(voluntary_presence_constraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a philosophical perspective, some asymmetry between partners may be inherent to intimate relationships: vulnerability, identity fusion, asymmetric caregiving needs, and temporal path-dependency create structural pressures that no relationship can fully escape. This perspective risks naturalizing what is actually a contingent institutional arrangement — treating the suppression mechanisms as inevitable rather than as engineered exit barriers. FALSE SUMMIT RISK: the constraint benefits the controlling partner, and this structure is not a law of nature but a choice-architecture.
constraint_indexing:constraint_classification(voluntary_presence_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: IDENTITY-LOCKED ANALYST (TANGLED ROPE) — An analyst embedded in relationship institutions (therapist, counselor, mediator, family law expert) whose professional identity is constituted through the relationship framework faces a reflexive version of the constraint: their capacity to see the extraction mechanisms is occluded by identity fusion with the institution they are theorizing. They experience genuine coordination function (relationships do provide care, intimacy, project alignment) alongside structural extraction, and the identity lock prevents them from fully exiting the frame even when analysis reveals it. This perspective instantiates the oracle gap: the framework's structure prevents the framework's users from seeing the structure itself.
constraint_indexing:constraint_classification(voluntary_presence_constraint, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(voluntary_presence_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(voluntary_presence_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(voluntary_presence_constraint, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(voluntary_presence_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(voluntary_presence_constraint, TR),
    TR >= 0.70.

:- end_tests(voluntary_presence_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The controlling partner extracts presence, emotional labor, financial consolidation, and identity regulation from the dependent partner, who receives minimal net benefit (or benefit conditioned on compliance). The extractiveness is not as extreme as a pure snare (0.72+) because many relationships contain genuine coordination elements — mutual care, shared projects, authentic intimacy — that coexist with the extraction mechanism. The measure reflects the dominant structure: presence is primarily compelled rather than chosen, but the relationship retains enough genuine coordination function to classify as Tangled Rope rather than pure Snare. The rising trajectory over time models the progressive deepening of financial coupling, children, and identity fusion that tightens the constraint. Suppression (0.68): High. Multiple barriers to exit operate simultaneously: economic (financial dependency, shared assets, income loss), social (network dismantling, custody barriers, public shame), identity (self-concept fused with relationship role), legal (asset division, spousal support obligations), and cognitive (identity lock preventing perception of exit as possible or desirable). The suppression is structural — engineered through choice-architecture rather than explicit coercion — and thus invisible as suppression: the dependent partner experiences barriers not as imposed constraints but as natural consequences of love and commitment. Theater ratio (0.55): Moderate. The relational narrative (presence as choice, love, commitment) diverges from structural reality (presence compelled by exit costs), but the gap is not as large as in purely performative institutions (marriage_institution at 0.70). Many relationships do provide genuine intimacy and care; the theater is not complete fabrication but a cover story layered over extraction. The theater rises slightly over time as partners develop a shared narrative that justifies the arrangement ('this is what love requires', 'sacrifice is natural', 'this is how relationships work').
 *
 * PERSPECTIVAL GAP:
 *   The gap between the controlling partner's rope classification and the dependent partner's snare classification reveals the constraint's core extractive structure. The controlling partner experiences the constraint as coordination because they benefit from continued presence and do not pay the suppression cost. The dependent partner experiences snare because they bear the full suppression cost and receive minimal benefit (or benefit conditioned on compliance). This perspectival inversion is the diagnostic signature of extraction disguised as coordination. The scaffold perspective's sunset logic (legal/social reforms reducing exit costs) predicts that as suppression decreases, the constraint should reclassify toward rope or piton (coordination or degraded ritual) if the controlling partner's benefits are genuine coordination, or persist as snare/tangled_rope if the extraction mechanism is the core function. The mountain perspective's naturalization risk reveals how philosophy can legitimize choice-architecture by treating contingent institutional arrangements as immutable properties of intimate bonding.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation chains beneficiary/victim declarations + exit options + power level → d value → f(d) → effective extraction chi. Controlling partner: beneficiary + arbitrage + institutional → d ≈ 0.20 → f(d) ≈ 0.02 (negative chi: they experience the constraint as beneficial). Dependent partner: victim + trapped + powerless → d ≈ 0.95 → f(d) ≈ 1.42 (maximum chi: they experience maximum extraction). Moderate partner: victim + constrained + moderate → d ≈ 0.65 → f(d) ≈ 1.00 (moderate chi: costs real but not insurmountable). Scaffold agents: mixed (advocate for victims, benefit from reducing constraint) + constrained + organized → d ≈ 0.40 → f(d) ≈ 0.40 (scaffold-level chi, lowered by agency). No directionality overrides needed — the derivation chain captures the structural relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandatrophy resolves through the perspectival landscape: there is no single 'correct' classification because different observers occupy genuinely different structural positions. The controlling partner's rope is their actual experience (they benefit, coordinate successfully, suppress minimally). The dependent partner's snare is their actual experience (they are trapped, extract nothing, bear full suppression). The scaffold's sunset logic is a real structural feature (legal/social reforms do reduce exit costs). The piton's degradation is real (the marriage institution has lost original function). The mountain's naturalization risk is a real epistemic trap. The identity-locked analyst's oracle gap is real (they cannot see structure their identity is constituted through). All six types are true from their respective observation points; the constraint's nature is the presheaf of these truths across the observation site. The mandatrophy does not resolve to 'which type is correct?' but to 'what is the structure that makes all these perspectival readings coherent?' The answer is: the voluntary presence constraint is a choice-architecture that appears voluntary from the beneficiary's frame, compulsory from the victim's frame, and reformable from the organized agent's frame, while risking naturalization from institutional/philosophical frames and remaining partially invisible to those whose identities are constituted through it. This is not ambiguity but structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exit_cost_threshold_ambiguity,
    'What exit cost threshold converts ''high-cost choice'' (constrained) into ''structural compulsion'' (trapped)? Is this threshold absolute or context-dependent?',
    'Cross-cultural longitudinal data on partner presence given varying exit cost architectures (economic, social, legal, identity); comparison of presence rates when exit costs are actively reduced (divorce law reform, economic support programs) vs. held constant',
    'If threshold is cultural/political (not natural): the voluntary presence constraint is a choice-architecture that can be redesigned. If threshold is invariant: some level of compulsion is inherent to intimate bonding. Determines whether Piton/Mountain perspectives accurately represent constancy or whether they mask engineered compulsion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_threshold_ambiguity, empirical, 'Threshold at which exit costs convert choice into compulsion').

omega_variable(
    relationship_authenticity_definition,
    'What constitutes ''authentic presence'' in intimate relationships? Can presence be simultaneously voluntary and constrained?',
    'Philosophical analysis of agency under constraint; empirical study of partner self-reported motivation (choice/love vs. obligation/fear) and comparison with behavioral indicators (intimacy, caregiving, coordination investment); longitudinal study of presence and intimacy quality before/after exit-cost reduction',
    'If authentic presence requires zero exit costs: most relationships are structurally inauthentic (constrained presence misnamed as love). If presence can be authentic despite high costs: the constraint may be reframed as a coordination mechanism requiring transparency rather than suppression. Determines whether the constraint is inherently extractive or contingently configured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(relationship_authenticity_definition, conceptual, 'Definition and conditions of authentic relational presence').

omega_variable(
    controlling_partner_agency_separation,
    'Are controlling behaviors (exit-cost engineering, isolation, financial control, emotional manipulation) a distinct constraint from the voluntary presence constraint, or are they the same constraint viewed through different exit-option lenses?',
    'Decomposition analysis: write separate stories for (a) the exit-cost architecture imposed by controlling partners, (b) the identity-fusion mechanism binding the dependent partner, (c) the coordination function that genuine relationships provide. Compare epsilon values across these stories. If epsilon differs significantly: three separate constraints. If epsilon converges: one constraint with multiple manifestations.',
    'If separate: controlling behavior is a snare that can exist independently of voluntary-presence ambiguity. If unified: the voluntary presence constraint IS the controlling behavior pattern viewed from the beneficiary''s frame. Determines whether to decompose this story into a constraint family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(controlling_partner_agency_separation, empirical, 'Whether controlling behavior and voluntary presence are one constraint or multiple').

omega_variable(
    identity_locked_suppression_internalization,
    'In relationships where identity fusion is the primary suppression mechanism (vs. material barriers), does the measured suppression persist after structural barriers are removed?',
    'Longitudinal study of partners after divorce: does suppression decrease immediately (suggesting structural mechanism) or persist (suggesting internalized suppression that travels with the partner post-exit)? Distinction between structural suppression (exit barriers) and internalized suppression (self-imposed constraints learned from the relationship).',
    'If suppression is primarily structural: reducing exit costs (economic support, housing, legal aid) resolves the constraint. If suppression is internalized: the constraint''s true mechanism is cognitive capture that survives material liberation. Determines whether the scaffold perspective adequately captures the sunset clause or whether identity-locked partners require different intervention architectures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_suppression_internalization, empirical, 'Internalized vs. structural suppression in relationships').

omega_variable(
    false_summit_natural_law_risk,
    'Is the asymmetry and vulnerability inherent to intimate bonding a law of relationship nature, or a contingent institutional arrangement that appears natural only because its alternatives are suppressed?',
    'Comparative analysis: examine relationships in contexts with minimal exit costs (decoupled economics, legal parity, social support infrastructure) vs. high exit costs. If asymmetry persists in low-cost contexts: suggests natural inevitability. If asymmetry vanishes: suggests engineered architecture.',
    'If natural: Mountain and Piton perspectives are accurate; some suppression is inescapable. If engineered: the mountain classification is a false summit (naturalization of choice architecture). Determines whether the constraint''s naturalness claim withstands scrutiny.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, empirical, 'Whether relationship asymmetry is natural or engineered').

omega_variable(
    care_coordination_genuine_vs_compelled,
    'In tangled-rope relationships where both coordination and extraction occur, how much of the caregiving and intimacy is genuine coordination responding to mutual vulnerability, vs. how much is compelled performance of care-by-trapped-partners?',
    'Qualitative study of caregiving patterns in relationships with varying exit-cost architectures; measurement of reciprocity, responsiveness, and self-reported motivation; comparison of care quality in high-suppression vs. low-suppression relationship contexts',
    'If caregiving is primarily genuine coordination even under high exit costs: Tangled Rope classification is accurate; extraction is modest. If caregiving is primarily compelled performance: the extraction metric is underestimated, and the constraint should classify higher (toward Snare). Determines whether the Tangled Rope type misses the extractive core by crediting coordination that is actually coerced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(care_coordination_genuine_vs_compelled, empirical, 'Proportion of caregiving that is genuine vs. compelled').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(voluntary_presence_constraint, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(volpres_tr_t0, voluntary_presence_constraint, theater_ratio, 0, 0.48).
narrative_ontology:measurement(volpres_tr_t3, voluntary_presence_constraint, theater_ratio, 3, 0.52).
narrative_ontology:measurement(volpres_tr_t6, voluntary_presence_constraint, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(volpres_be_t0, voluntary_presence_constraint, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(volpres_be_t3, voluntary_presence_constraint, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(volpres_be_t6, voluntary_presence_constraint, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(volpres_su_t0, voluntary_presence_constraint, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(volpres_su_t3, voluntary_presence_constraint, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(volpres_su_t6, voluntary_presence_constraint, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(voluntary_presence_constraint, attachment_coordination).
narrative_ontology:affects_constraint(voluntary_presence_constraint, control_isolation_mechanism).
narrative_ontology:affects_constraint(voluntary_presence_constraint, financial_coupling_asymmetry).
narrative_ontology:affects_constraint(voluntary_presence_constraint, identity_fusion_cognitive_capture).

% DUAL FORMULATION NOTE:
% The voluntary presence constraint should decompose into three structurally distinct stories: (1) voluntary_presence_constraint (this story): the overarching choice-architecture ambiguity; (2) control_isolation_mechanism (ε ≈ 0.72, Snare): the suppression machinery (financial control, network dismantling, emotional manipulation) as a pure extraction constraint; (3) identity_fusion_cognitive_capture (ε ≈ 0.65, Snare): the internalization of suppression through identity lock, separate from material barriers. Each story has different epsilon, different omegas, different measurement patterns. The family structure captures how exit-cost architecture (this story) depends on enforcement mechanisms (control_isolation) and cognitive capture (identity_fusion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
