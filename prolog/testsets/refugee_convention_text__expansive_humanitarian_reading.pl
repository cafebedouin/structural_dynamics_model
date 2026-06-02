% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__expansive_humanitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__expansive_humanitarian_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: refugee_convention_text__expansive_humanitarian_reading
 *   human_readable: Refugee Convention Text — Expansive Humanitarian Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   The expansive humanitarian reading of the 1951 Refugee Convention
 *   interprets the definition of a refugee to include persons fleeing
 *   generalized violence and non-state persecution, and recognizes
 *   'particular social group' as encompassing gender-based persecution,
 *   LGBTQ+ persecution, and clan-based violence. This reading instantiates
 *   one committed interpretation of an ambiguous kernel text, competing with
 *   a restrictive sovereignty-focused reading and a procedural integrity
 *   reading. The expansive reading has generated the core institutional
 *   architecture of contemporary asylum systems: broad victim categories,
 *   substantive claim assessment, non-refoulement as absolute principle, and
 *   recognition of persecution by non-state actors and structural violence.
 *   The constraint exhibits characteristics of both coordination (states have
 *   incentive to share burden-sharing frameworks) and extraction (broad
 *   victim categories impose resource costs on receiving states while
 *   creating asymmetric benefits for advocacy organizations and states with
 *   labor demand). Theater_ratio is moderate (0.42) because the institutional
 *   protection system maintains genuine substantive assessment procedures,
 *   unlike systems where assessment is purely performative. However, rising
 *   theater_ratio over time reflects growing awareness that extraction
 *   mechanisms (interdiction, offshore processing, pushbacks) render formal
 *   recognition partially theatrical — the assessments occur but their
 *   protective effect is circumscribed by extraterritorial gatekeeping.
 *   Suppression_requirement rises over time as states build enforcement
 *   capacity to manage asylum flows despite formal commitment to the
 *   expansive reading.
 *
 * KEY AGENTS:
 *   - Asylum Seekers Fleeing Generalized Violence: Primary victims (powerless/trapped) — experience the expansive reading as sole pathway to protection; bear full cost of restrictive interpretation
 *   - LGBTQ+ Persons and Gender-Persecuted Individuals: Primary victims (powerless/identity_locked) — identity-based persecution only recognized under expansive reading; cannot abandon the identity that triggers persecution
 *   - Border and Receiving States: Primary extractors (institutional/constrained or institutional/arbitrage) — face coordination problem (burden-sharing) and resource costs (processing, integration); also benefit from selective recruitment of skilled refugees
 *   - Human Rights Advocacy Organizations: Secondary beneficiaries (organized/mobile) — advocate for expansive reading; benefit from funding, legitimacy, and organizational mission tied to broad protection
 *   - Wealthier Destination States with Labor Demand: Tertiary beneficiaries (institutional/arbitrage) — benefit from asylum flows as source of skilled labor; maintain humanitarian legitimacy through broad interpretation
 *   - Immigration Enforcement Systems: Institutional victim (institutional/constrained) — face mandate to assess all asylum claims substantively despite resource constraints and political pressure for restrictive outcomes
 *   - International Protection System Complex: Piton agent (institutional/constrained) — maintains theatrical assessment procedures despite widespread knowledge that extraction mechanisms undermine protective effect
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, 0.58).
domain_priors:suppression_score(refugee_convention_text__expansive_humanitarian_reading, 0.68).
domain_priors:theater_ratio(refugee_convention_text__expansive_humanitarian_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__expansive_humanitarian_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__expansive_humanitarian_reading, "Refugee Convention Text — Expansive Humanitarian Reading").
narrative_ontology:topic_domain(refugee_convention_text__expansive_humanitarian_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__expansive_humanitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__expansive_humanitarian_reading, 'f409a60b-6b52-4915-8728-d6485b76a778').
narrative_ontology:cs_kernel_codification('f409a60b-6b52-4915-8728-d6485b76a778', fixed_text).
narrative_ontology:cs_authority_grounding('f409a60b-6b52-4915-8728-d6485b76a778', lineage).
narrative_ontology:cs_interpretation_layer_present('f409a60b-6b52-4915-8728-d6485b76a778').
narrative_ontology:cs_reading_relation('f409a60b-6b52-4915-8728-d6485b76a778', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('f409a60b-6b52-4915-8728-d6485b76a778', refugee_convention_text__procedural_integrity_reading, influences).
narrative_ontology:cs_axiom('f409a60b-6b52-4915-8728-d6485b76a778', foundational, persecution_includes_generalized_violence).
narrative_ontology:cs_axiom_status(persecution_includes_generalized_violence, holdable).
narrative_ontology:cs_axiom_grounding('f409a60b-6b52-4915-8728-d6485b76a778', persecution_includes_generalized_violence, empirically_contingent).
narrative_ontology:cs_axiom('f409a60b-6b52-4915-8728-d6485b76a778', foundational, particular_social_group_includes_gender_and_sexual_orientation).
narrative_ontology:cs_axiom_status(particular_social_group_includes_gender_and_sexual_orientation, holdable).
narrative_ontology:cs_axiom_grounding('f409a60b-6b52-4915-8728-d6485b76a778', particular_social_group_includes_gender_and_sexual_orientation, deontological).
narrative_ontology:cs_axiom('f409a60b-6b52-4915-8728-d6485b76a778', secondary, non_state_actor_persecution_triggers_obligation).
narrative_ontology:cs_axiom_status(non_state_actor_persecution_triggers_obligation, holdable).
narrative_ontology:cs_axiom_grounding('f409a60b-6b52-4915-8728-d6485b76a778', non_state_actor_persecution_triggers_obligation, empirically_contingent).
narrative_ontology:cs_reference_frame('f409a60b-6b52-4915-8728-d6485b76a778', universal_humanitarian_protection_mandate).
narrative_ontology:cs_drift_state('f409a60b-6b52-4915-8728-d6485b76a778', contemporary_securitized_borders, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f409a60b-6b52-4915-8728-d6485b76a778', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_fleeing_persecution).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, human_rights_advocacy_organizations).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, state_fiscal_capacity).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, immigration_enforcement_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASYLUM SEEKER / TRAPPED (SNARE) — Faces generalized violence in origin state (gang violence, femicide, sectarian conflict) with no meaningful exit within origin territory. Trapped by structural vulnerability. From this agent's perspective, the expansive reading is the only reading that recognizes their claim as legitimate. The constraint appears as pure extraction only if denied — the reading itself provides the sole pathway to safety. Maximum suppression, minimal alternatives.
constraint_indexing:constraint_classification(refugee_convention_text__expansive_humanitarian_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LGBTQ+ PERSON / IDENTITY_LOCKED (SNARE) — Faces persecution based on identity that cannot be abandoned without existential self-negation. The expansive reading is structurally necessary for this agent's claim to be recognized — the 'particular social group' category must encompass sexual orientation and gender identity. Structurally mobile (could remain in origin state while hiding) but identity_locked (cannot hide without dissolution of self). From this perspective, the constraint's protective scope is non-negotiable.
constraint_indexing:constraint_classification(refugee_convention_text__expansive_humanitarian_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: BORDER STATE / CONSTRAINED (TANGLED ROPE) — Faces genuine coordination problem: managing asylum flows while maintaining border security and public order. The expansive reading imposes real costs (processing burden, integration needs, political opposition). But also genuine benefits: humanitarian legitimacy, access to international cooperation on return and reintegration, refugees as labor source in tight markets. Constrained exit — cannot simply reject all claims (violates international law, triggers sanctions), cannot process all claims at zero cost (resource limits). Mixed coordination and extraction.
constraint_indexing:constraint_classification(refugee_convention_text__expansive_humanitarian_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: HUMAN RIGHTS ADVOCACY / MOBILE (SCAFFOLD) — Organized coalition (UNHCR, Amnesty International, national refugee councils) advocating for broad interpretation as a temporary corrective to restrictive state practices. From this perspective, the expansive reading is a scaffold: a set of norms and procedures designed to sunset as state practices converge toward humanitarian standards. Sees the constraint as enforcement mechanism with a generational sunset — as states normalize asylum processing, the need for aggressive interpretation diminishes.
constraint_indexing:constraint_classification(refugee_convention_text__expansive_humanitarian_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: DESTINATION STATE / ARBITRAGE (ROPE) — Wealthy states with labor shortages experience the expansive reading as coordination mechanism: asylum categories that capture skilled refugees (medical professionals, engineers fleeing persecution or generalized violence) enable selective recruitment while maintaining humanitarian legitimacy. Net beneficiary through arbitrage (admit high-skill refugees while appearing humanitarian). Low suppression, high degrees of freedom in claim evaluation.
constraint_indexing:constraint_classification(refugee_convention_text__expansive_humanitarian_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INSTITUTIONAL PROTECTION SYSTEM / CONSTRAINED (PITON) — The expansive reading is maintained through institutional inertia and regulatory continuity even as its functional protection capacity has degraded. Theater_ratio high: elaborate assessment procedures, appeal mechanisms, and evidence standards persist despite widespread knowledge that extraction mechanisms (interdiction, offshore processing, pushbacks) render formal recognition largely performative. The constraint persists because dismantling it would expose the fiction — easier to maintain theatrical full implementation than to openly reformulate.
constraint_indexing:constraint_classification(refugee_convention_text__expansive_humanitarian_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / UNIVERSAL (MOUNTAIN — FALSE SUMMIT) — From a civilizational perspective, the expansive humanitarian reading appears as a natural law of human moral reasoning: persecution is inherently wrong, protection is inherently obligatory, and broad definitions of persecution follow logically from the core commitment to non-refoulement. This perspective risks naturalizing what is actually a contested institutional reading of an ambiguous text. The false summit detector will flag this as naturalization of a particular reading, not discovery of an immutable principle.
constraint_indexing:constraint_classification(refugee_convention_text__expansive_humanitarian_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(refugee_convention_text__expansive_humanitarian_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(refugee_convention_text__expansive_humanitarian_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(refugee_convention_text__expansive_humanitarian_reading, TR),
    TR >= 0.70.

:- end_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The expansive reading imposes substantial costs on receiving states (processing burden, integration demand, political opposition, enforcement complexity) while generating benefits for asylum seekers and advocacy organizations. The extraction is not maximal because genuine coordination problems exist (burden-sharing, capacity limits) and significant beneficiaries (labor-market states) experience net gain. Rising trajectory (0.35 → 0.58 over 20-year interval) reflects accumulating extraction as: (1) victim definitions have expanded to include gang violence, femicide, and LGBTQ+ persecution; (2) state capacity for offshore processing and interdiction has increased, creating gap between formal obligation and actual protection; (3) advocacy organizations have professionalized, creating institutional vested interest in maintaining broad interpretations. Suppression (0.68): High and rising. Structural barriers to asylum recognition include: (1) evidentiary burden of proving persecution and state inability/unwillingness to protect; (2) geographic barriers (interdiction, transit restrictions, visa requirements); (3) procedural barriers (complex claims assessment, language barriers, legal representation gaps); (4) political barriers (domestic opposition, border securitization, anti-refugee discourse). Rising trajectory reflects state investment in enforcement machinery designed to suppress claims at point of entry rather than assessing them substantively. Theater_ratio (0.42): Moderate and stable. The expansive reading generates genuine substantive assessment procedures — asylum interviews, credibility findings, country condition analysis — that have actual protective function. Theater is moderate because the assessment is real (not purely performative ritual) but increasingly circumscribed by extraction mechanisms that operate outside the assessment framework (interdiction, offshore processing, pushbacks). Suppression_requirement rising while theater_ratio stable suggests that states are increasingly using gatekeeper enforcement rather than hollow assessments to manage the constraint's extraction burden.
 *
 * PERSPECTIVAL GAP:
 *   The expansive reading generates a profound perspectival gap between powerless agents (asylum seekers, LGBTQ+ individuals) who experience it as the only coherent protection mechanism and constrained/arbitrage institutional agents (border states, wealthier destination states) who experience it as an extraction mandate. The asylum seeker trapped by generalized violence and the LGBTQ+ person identity_locked by persecution see the expansive reading as non-negotiable — not because they benefit, but because restrictive readings render their persecution invisible and unprotectable. The border state sees the same reading as an obligation-extraction: genuine coordination problem (burden-sharing, capacity limits) mixed with asymmetric cost imposition. The advocacy coalition sees a temporary scaffold with a generational sunset as state practices normalize. The destination state with labor demand sees a coordination mechanism enabling selective recruitment. The institutional protection system sees degraded theater — assessments that are increasingly performative because extraction mechanisms circumvent them. The analytical observer risks naturalizing the expansive reading as a law of human morality rather than recognizing it as one contested interpretation of an ambiguous text.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from the structural relationship of each agent to the expansive reading. Powerless/trapped agents fleeing persecution have maximum d (1.0) not because they benefit, but because they bear the full cost of restrictive interpretation — their exit capacity is zero and the constraint's protective scope directly determines their survival prospects. LGBTQ+ persons with identity_locked exit have high d (0.89) — structurally mobile (could hide) but identity-bound (cannot exit without self-negation). Border states constrained by resource limits and political opposition have moderate d (0.55) — can exercise some discretion in claim assessment but cannot simply reject all claims. Wealthier destination states with arbitrage options have low d (0.15) — benefit from selective recruitment while maintaining humanitarian legitimacy, multiple exit options (tighten borders, process selectively, offshore-process). Advocacy organizations with mobile exit have low d (0.15) — benefit from broad interpretations, can defund or relocate if political context shifts. The piton institutional system has moderate d (0.50) — neither benefits nor bears maximum cost; perpetuates the constraint through inertia. The sigmoid f(d) function amplifies extraction experienced by powerless agents and dampens it for institutional beneficiaries, creating the perspectival divergence in classification types.
 *
 * MANDATROPHY ANALYSIS:
 *   The expansive reading resolves mandatrophy by clarifying that the constraint is fundamentally mixed coordination-extraction (tangled rope), not pure extraction or pure coordination. The coordination function is genuine: state burden-sharing frameworks, international cooperation, and mutual recognition of persecution definitions solve collective action problems around asylum processing. The extraction function is also genuine: the expansive victim definitions impose asymmetric costs on border/resource-limited states while distributing benefits to advocacy organizations, labor-market states, and asylum seekers. The mandatrophy dissolves when we ask: 'From which perspective?' From the powerless asylum seeker's perspective, the constraint is snare — pure extraction relative to being denied protection. From the destination state's perspective with labor demand, the constraint is rope — coordination enabling selective recruitment. From the border state's perspective, the constraint is tangled rope — mixed burden (resource cost, political opposition) with some benefit (international legitimacy, access to return frameworks). No single type is 'correct' — the constraint's classification is perspectival.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generalized_violence_threshold,
    'What level of generalized violence in a territory triggers ''well-founded fear of persecution'' under the expansive reading vs. requiring state-targeted persecution?',
    'Case law analysis: track judicial decisions distinguishing persecution from generalized violence; identify operative thresholds in UN guidance documents and state asylum decisions; comparative analysis of acceptance rates for gang violence, femicide, and sectarian violence claims',
    'If threshold is low (generalized violence sufficient): expands victim set dramatically, reclassifies constraint as pure extraction from state perspective. If threshold is high (state-targeted only): collapses back toward restrictive reading, narrows victim set, shifts constraint toward rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generalized_violence_threshold, empirical, 'Definitional threshold for generalized violence as persecution').

omega_variable(
    particular_social_group_scope,
    'Does ''particular social group'' encompass gender, sexual orientation, clan membership, and caste — or require narrower definitions based on immutability or social visibility?',
    'UNHCR guidance documents; comparative jurisprudence (EU, Canada, Australia, US); empirical tracking of acceptance rates for gender-based violence, LGBTQ+, clan-based, and caste-based claims under different interpretive frameworks',
    'Broad scope (gender, sexual orientation, clan, caste): victim set includes millions globally; constraint reclassifies as snare from state perspective. Narrow scope: reduces victim set to <1M; constraint shifts toward rope or scaffold classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(particular_social_group_scope, conceptual, 'Definitional scope of ''particular social group'' category').

omega_variable(
    non_state_actor_liability,
    'Does the 1951 Convention obligate states to grant asylum for persecution by non-state actors (gangs, cults, private militias) when the state cannot or will not provide protection?',
    'Text analysis: Article 1(A)(2) does not specify state actor; UNHCR Handbook explicitly includes non-state persecution; empirical tracking of judicial decisions on non-state actor claims; state practice analysis on interdiction and rejection rationales',
    'If non-state liability included: victim set doubles (generalized gang violence, cult persecution, private militia violence now recognized); constraint becomes maximum-extraction snare from state sovereignty perspective. If excluded: returns to pre-1980s narrow state-targeted interpretation; constraint reclassifies as coordination rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_state_actor_liability, empirical, 'Whether non-state actor persecution triggers Convention obligations').

omega_variable(
    offshore_processing_refoulement_equivalence,
    'Do offshore processing centers, interception at sea, and transit zone detention constitute refoulement (or indirect refoulement) under the expansive reading?',
    'International Court jurisprudence (ICJ, regional courts); UNHCR position papers; empirical tracking of returns from offshore processing; comparative analysis of state practices and judicial challenges',
    'If offshore = refoulement: renders entire interdiction apparatus illegal under expansive reading; transforms constraint into pure extraction snare from state perspective; forces reconceptualization of border control. If offshore ≠ refoulement: preserves state capacity for gatekeeping; constraint shifts toward tangled rope (mixed coordination and limited extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(offshore_processing_refoulement_equivalence, empirical, 'Whether offshore processing constitutes prohibited refoulement').

omega_variable(
    reading_kernel_committer_ambiguity,
    'Is the 1951 Convention text genuinely ambiguous on generalized violence, non-state persecution, and social group scope — or does the restrictive reading represent a legitimate alternative interpretation of a determinate kernel?',
    'Textual analysis: compare plain-language reading of Art. 1(A)(2) with travaux préparatoires (drafting history); assess whether restrictive reading can be defended as consistent with original intent; examine whether expansive reading requires creative interpretation or follows naturally from text',
    'If genuinely ambiguous: both readings are defensible; constraint is a coexists_with relationship between readings. If restrictive reading is parasitic on expansive: expansive forecloses restrictive; constraint becomes single stable interpretation. If expansive requires creative reinterpretation: might signal that the reading is driven by political advocacy rather than textual analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_committer_ambiguity, conceptual, 'Whether the Convention text is genuinely ambiguous or the readings represent competing interpretations').

omega_variable(
    humanitarian_sovereignty_boundary,
    'Where is the legitimate boundary between humanitarian obligation (expansive reading) and state sovereignty (restrictive reading)? Is this boundary located in the text, in state capacity, in moral principle, or in negotiated international consensus?',
    'Philosophical analysis: examine how different moral frameworks (cosmopolitan, nationalist, pluralist) ground the boundary; empirical analysis of state capacity constraints; comparative jurisprudence on where different states locate the boundary; tracking of international norm evolution',
    'If located in text: expansive reading is correct; constraint is stable. If located in state capacity: boundary shifts with economic conditions and labor market needs; constraint becomes contingent. If located in moral principle: boundary is non-negotiable but may conflict with state interests (snare classification confirmed). If located in consensus: boundary is politically constructed; constraint may be subject to renegotiation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(humanitarian_sovereignty_boundary, preference, 'Philosophical and political location of humanitarian obligation boundary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__expansive_humanitarian_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refcon_exp_tr_t0, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(refcon_exp_tr_t10, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(refcon_exp_tr_t20, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(refcon_exp_be_t0, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(refcon_exp_be_t10, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(refcon_exp_be_t20, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(refcon_exp_su_t0, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(refcon_exp_su_t10, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(refcon_exp_su_t20, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__expansive_humanitarian_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__procedural_integrity_reading).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, asylum_processing_capacity_bottleneck).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, non_refoulement_absolute_principle).

% DUAL FORMULATION NOTE:
% This constraint is one reading (expansive_humanitarian_reading) of the contested kernel refugee_convention_text. Sibling readings instantiate the same Convention text but reach different classifications of what counts as persecution, what counts as a social group, and what constitutes state obligation. All three readings (expansive, restrictive, procedural) emerge from the same legal text; the differences are not measurement artifacts but genuine interpretive alternatives grounded in different priorities (humanitarian obligation vs. sovereignty vs. procedural feasibility). Each reading has its own ε, its own perspectives, and its own CS structure entries recording the reading relations and axioms. The sibling readings are linked via network.affects_constraints in all three story files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(refugee_convention_text__expansive_humanitarian_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
