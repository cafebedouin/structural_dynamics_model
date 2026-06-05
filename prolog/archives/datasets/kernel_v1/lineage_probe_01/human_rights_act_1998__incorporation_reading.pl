% ============================================================================
% CONSTRAINT STORY: human_rights_act_1998__incorporation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_rights_act_1998__incorporation_reading, []).

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
 *   constraint_id: human_rights_act_1998__incorporation_reading
 *   human_readable: HRA Incorporation: Domestic Rights Enforcement (Incorporation Reading)
 *   domain: constitutional_law/human_rights
 *
 * SUMMARY:
 *   The Human Rights Act 1998 incorporated the European Convention on Human
 *   Rights into English law, making Convention rights enforceable directly in
 *   domestic courts. This incorporation reading interprets the HRA as a
 *   fundamentally coordinative mechanism: it solves the collective action
 *   problem of enforcing rights across a two-tier system (domestic law +
 *   international oversight) by channeling most remedies through domestic
 *   courts, reserving Strasbourg for systemic compliance review. Under this
 *   reading, the 'long road to Strasbourg' is no longer a necessary path for
 *   individual rights vindication—it begins with domestic remedies available
 *   in any court. This contrasts sharply with rival readings: the
 *   judicial_power_grab_reading views the HRA as a covert transfer of
 *   sovereignty to judges through expansive section 3 interpretation; the
 *   parliamentary_sovereignty_preserved_reading emphasizes Parliament's
 *   retained power to override via section 4 declarations of incompatibility.
 *   The incorporation reading is one coherent doctrinal position within the
 *   broader contest over the HRA's constitutional meaning. The constraint
 *   metrics reflect that under this reading, extractiveness is low (remedies
 *   are genuinely available domestically, suppression is reduced, and theater
 *   is moderate because some procedural complexity remains but the framework
 *   is functionally transparent).
 *
 * KEY AGENTS:
 *   - Rights Claimants: Primary beneficiaries under incorporation reading (powerless/mobile) — gain access to domestic remedies without Strasbourg travel extraction
 *   - Domestic Courts: Institutional beneficiary (institutional/arbitrage) — exercise interpretive authority via section 3; coordinate rights enforcement at first instance
 *   - Rights-Violating Public Authorities: Victims of the constraint structure (powerful/constrained) — face accountability through both section 3 interpretation and section 4 declarations; can no longer claim rights obligations are unclear
 *   - Parliament: Institutional actor (institutional/arbitrage) — retains formal override capacity via section 4, but under incorporation reading, this power is residual rather than primary
 *   - Strasbourg Court: Institutional actor (institutional/arbitrage) — transformed from primary rights-enforcement venue to secondary systemic review forum under incorporation logic
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the HRA as a coordinative solution distributing remedies across two tiers with clear boundaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_rights_act_1998__incorporation_reading, 0.28).
domain_priors:suppression_score(human_rights_act_1998__incorporation_reading, 0.35).
domain_priors:theater_ratio(human_rights_act_1998__incorporation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_rights_act_1998__incorporation_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(human_rights_act_1998__incorporation_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(human_rights_act_1998__incorporation_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_rights_act_1998__incorporation_reading, rope).
narrative_ontology:human_readable(human_rights_act_1998__incorporation_reading, "HRA Incorporation: Domestic Rights Enforcement (Incorporation Reading)").
narrative_ontology:topic_domain(human_rights_act_1998__incorporation_reading, "constitutional_law/human_rights").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_rights_act_1998__incorporation_reading, '18e4641e-5805-4c27-806d-7081d29ac6fc').
narrative_ontology:cs_kernel_codification('18e4641e-5805-4c27-806d-7081d29ac6fc', formalized).
narrative_ontology:cs_authority_grounding('18e4641e-5805-4c27-806d-7081d29ac6fc', lineage).
narrative_ontology:cs_interpretation_layer_present('18e4641e-5805-4c27-806d-7081d29ac6fc').
narrative_ontology:cs_reading_relation('18e4641e-5805-4c27-806d-7081d29ac6fc', human_rights_act_1998__judicial_power_grab_reading, coexists_with).
narrative_ontology:cs_reading_relation('18e4641e-5805-4c27-806d-7081d29ac6fc', human_rights_act_1998__parliamentary_sovereignty_preserved_reading, coexists_with).
narrative_ontology:cs_axiom('18e4641e-5805-4c27-806d-7081d29ac6fc', foundational, section_3_interprets_rather_than_rewrites).
narrative_ontology:cs_axiom_status(section_3_interprets_rather_than_rewrites, holdable).
narrative_ontology:cs_axiom_grounding('18e4641e-5805-4c27-806d-7081d29ac6fc', section_3_interprets_rather_than_rewrites, deontological).
narrative_ontology:cs_axiom('18e4641e-5805-4c27-806d-7081d29ac6fc', foundational, remedy_gap_closed_by_domestic_access).
narrative_ontology:cs_axiom_status(remedy_gap_closed_by_domestic_access, holdable).
narrative_ontology:cs_axiom_grounding('18e4641e-5805-4c27-806d-7081d29ac6fc', remedy_gap_closed_by_domestic_access, empirically_contingent).
narrative_ontology:cs_reference_frame('18e4641e-5805-4c27-806d-7081d29ac6fc', two_tier_remedy_structure_with_domestic_primacy).
narrative_ontology:cs_drift_state('18e4641e-5805-4c27-806d-7081d29ac6fc', contemporary_post_2015_jurisprudence, gap(stable, minor, true)).
narrative_ontology:cs_created_at('18e4641e-5805-4c27-806d-7081d29ac6fc', '').
narrative_ontology:cs_kernel_id(human_rights_act_1998__incorporation_reading, human_rights_act_1998).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_rights_act_1998__incorporation_reading, rights_claimants).
narrative_ontology:constraint_beneficiary(human_rights_act_1998__incorporation_reading, domestic_courts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RIGHTS CLAIMANT (ROPE) — Under the incorporation reading, claimants access justice through domestic courts without the extraction cost of the Strasbourg journey. Mobile exit option reflects genuine remedy availability. The constraint coordinates remedial access — claimants and courts solve collective action problem of enforcing rights without procedural fragmentation. Beneficiary position with mobile options produces low experienced extraction.
constraint_indexing:constraint_classification(human_rights_act_1998__incorporation_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: RIGHTS-VIOLATING PUBLIC AUTHORITY (TANGLED ROPE) — Constrained by section 3 interpretive duty and section 4 declaration risk, yet also benefits from domestic clarification of rights obligations before Strasbourg escalation. The constraint coordinates public authority compliance while extracting accountability. Mixed coordination (clarification of duties) and extraction (liability exposure). Suppression is real (courts can declare incompatibility) but not total (Parliament retains override).
constraint_indexing:constraint_classification(human_rights_act_1998__incorporation_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMESTIC COURT SYSTEM (ROPE) — Institutional beneficiary with arbitrage (interpretive discretion via section 3). The constraint coordinates the distribution of rights remedies. Courts experience low extraction because they have agency in interpretation. The mechanism is pure coordination — enabling claimants to access rights without Strasbourg travel, enabling courts to exercise interpretive authority over statutory meaning.
constraint_indexing:constraint_classification(human_rights_act_1998__incorporation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STRASBOURG COURT / INTERNATIONAL RIGHTS REGIME (ROPE) — The incorporation reading presents Strasbourg not as an extraction mechanism but as a coordination node. Under this reading, the HRA channels most remedies through domestic courts, making Strasbourg a secondary forum for systemic compliance review rather than a primary rights-enforcement venue. The regime benefits from reduced caseload and enhanced legitimacy through domestic implementation. This is genuinely coordinative — the international regime and domestic systems share the burden of rights protection.
constraint_indexing:constraint_classification(human_rights_act_1998__incorporation_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From the analytical position, the HRA's incorporation mechanism is a pure coordination solution to the enforcement gap created by European membership. It distributes rights remedies across a two-tier system (domestic + Strasbourg) with clear jurisdictional boundaries. The mechanism reduces travel costs, speeds remedies, and preserves Parliament's formal authority. This is the incorporation reading's canonical analytical classification — the constraint solves a collective action problem (enforcing rights without creating gaps between Convention text and domestic implementation) with minimal coercion.
constraint_indexing:constraint_classification(human_rights_act_1998__incorporation_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_rights_act_1998__incorporation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(human_rights_act_1998__incorporation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(human_rights_act_1998__incorporation_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(human_rights_act_1998__incorporation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. Under the incorporation reading, the HRA removes the extraction cost of mandatory Strasbourg trips for individual claimants — they can access remedies domestically. The residual extractiveness reflects two factors: (1) procedural complexity remains (claimants must still navigate court systems, which requires resources and knowledge), and (2) courts retain interpretive discretion via section 3, creating some unpredictability in outcome. However, the major extraction mechanism (forced international travel + delay + specialized expense) is eliminated. The downward trajectory (0.35 → 0.28 over the interval) reflects empirical learning: as domestic courts develop settled jurisprudence on Convention rights, the unpredictability of section 3 interpretation decreases, and extractiveness falls. Suppression (0.35): Moderate. Courts are empowered to interpret statutes compatibly with rights and to declare incompatibility. This is not suppression of alternatives—it is clarification of them. Yet suppression is not zero: public authorities cannot simply ignore Convention rights, and Parliament cannot override declarations costlessly (even if override remains theoretically possible). The constraint structures the available options without eliminating them. Theater ratio (0.42): Moderate. Domestic courts apply section 3 with genuine attention to statutory meaning—this is not purely performative. Yet some theater persists: courts must formulate interpretations as readings of the statute rather than explicit rewrites, and section 4 declarations maintain the fiction of Parliamentary final authority even when Parliament rarely exercises it. The trajectory is slightly downward as jurisprudence stabilizes and the performative framing becomes less necessary.
 *
 * PERSPECTIVAL GAP:
 *   The incorporation reading produces less perspectival gap than the rival readings because its core claim is that the HRA is a coordinative solution from all perspectives. All five perspectives (claimants, authorities, courts, Strasbourg, analyst) classify as rope or low-extraction tangled_rope, reflecting that the incorporation reading views the constraint as solving a collective action problem rather than concentrating extraction. The gap would widen sharply if we added the judicial_power_grab_perspective (which would classify section 3 as snare from Parliament's view) or the parliamentary_sovereignty_perspective (which would emphasize residual Parliamentary authority, lowering extraction felt by Parliament). The incorporation reading's internal coherence depends on maintaining that section 3 interpretation is genuinely coordinative and that section 4 override, while rare, is a real safeguard. If either claim fails empirically (section 3 becomes routine rewriting, or section 4 becomes purely ceremonial), the perspectival gap expands and the incorporation reading collapses into one of its rivals.
 *
 * DIRECTIONALITY LOGIC:
 *   The incorporation reading's directionality derives from the empirical claim that the HRA genuinely provides domestic remedies to rights claimants. Under this reading: (1) Rights claimants are beneficiaries with mobile exit options—they can access courts, and courts will provide relief. High mobility + beneficiary status produces low d (approximately 0.20), yielding low experienced extraction via f(d). (2) Public authorities are victims of the constraint in a structural sense—they are now accountable for Convention compliance through domestic courts. However, the incorporation reading treats this accountability as legitimate (not as extraction but as proper enforcement), so the victim classification does not produce high d—the constraint coordinates legitimate accountability rather than extracting unfair advantage. (3) Courts are beneficiaries with institutional power and arbitrage—they exercise interpretive authority and are not subordinated. (4) Parliament retains formal override via section 4, preserving institutional arbitrage even if the power is rarely exercised. The directionality chain produces uniformly low effective extraction because the incorporation reading's core claim is that the mechanism genuinely serves all parties' interest in accessible rights enforcement without excessive concentration of power.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    section_3_interpretive_scope,
    'Does the section 3 ''reading down'' duty represent genuine coordination (clarifying statutory meaning to align with rights) or disguised judicial rewriting (rewriting statutes in all but name)?',
    'Case-law analysis: instances where courts apply section 3 and trace whether the result is interpretive (stretching plausible statutory meaning) or creative (imposing meaning the statute does not plainly bear). Empirical threshold: if >40% of section 3 cases require statutory text to be strained beyond ordinary meaning, the mechanism is closer to rewriting than interpretation.',
    'If genuinely interpretive coordination: rope classification confirmed, extractiveness ~0.28. If disguised rewriting: reclassify as tangled_rope or snare, extractiveness rises to 0.45-0.65, and the judicial_power_grab_reading becomes structurally more accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(section_3_interpretive_scope, empirical, 'Whether section 3 interpretation is coordinative or rewriting in disguise').

omega_variable(
    section_4_declaration_enforceability,
    'Do section 4 declarations of incompatibility function as merely advisory (Parliament can ignore them), or do they function as de facto strike-downs despite formal parliamentary override?',
    'Empirical tracking of Parliament''s response to declarations: proportion of declarations that are subsequently remedied vs ignored. Timeline analysis: speed of parliamentary correction, political cost of non-compliance, actual override instances (very rare in practice).',
    'If genuinely advisory (Parliament regularly overrides): parliamentary_sovereignty_preserved_reading is accurate, extraction is low, constraint is rope. If de facto strike-downs (Parliament rarely overrides despite formal power): judicial_power_grab_reading is accurate, extraction rises, constraint becomes snare of parliamentary authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(section_4_declaration_enforceability, empirical, 'Whether section 4 declarations are truly advisory or function as de facto strike-downs').

omega_variable(
    incorporation_vs_strasbourg_substitution,
    'Does domestic incorporation genuinely supplement Strasbourg review, or does it substitute for it, creating a gap when domestic remedies fail?',
    'Empirical analysis of cases reaching Strasbourg post-HRA: proportion that involve prior domestic remedies, nature of failures (procedural bars, substantive errors, lack of remedy availability). If high proportion of Strasbourg cases cite domestic remedy failure, the incorporation model has a systematic gap — victims still face the extraction cost of Strasbourg trip after exhausting domestic options.',
    'If genuine supplementation with low gap: incorporation thesis holds, extractiveness remains ~0.28. If systematic gap: extractiveness rises to 0.35-0.45, and a separate constraint (the remedial gap after domestic exhaustion) should be authored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incorporation_vs_strasbourg_substitution, empirical, 'Whether domestic incorporation truly replaces Strasbourg review or creates a systematic remedial gap').

omega_variable(
    reading_contest_foreclosure,
    'Can the incorporation reading coexist with the judicial_power_grab_reading within the same constitutional framework, or does one logically preclude the other?',
    'Doctrinal analysis: whether the two readings depend on contradictory premises about section 3''s scope and section 4''s enforceability. If both depend on empirically testable facts (how courts interpret, how Parliament responds) that differ by degree, coexistence is possible. If they depend on contradictory normative commitments (courts DO have rewriting power vs courts DO NOT), foreclosure is actual.',
    'If coexistence: both readings remain live, constraint family structure preserved. If foreclosure: one reading must give way, and the kernel contest is actually a cascade that resolves to one standing interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Logical compatibility of incorporation reading with judicial power grab reading').

omega_variable(
    parliament_last_word_mythic_status,
    'Is Parliament''s retained override capacity under section 4 a substantive safeguard, or has it become ceremonial—a formally preserved power that politically cannot be exercised?',
    'Empirical review of actual parliamentary responses to declarations; interviews with parliamentary draftspersons and legislative counsel about perceived political cost of override; comparison with other jurisdictions where override provisions are exercised vs dormant.',
    'If substantive safeguard: parliamentary_sovereignty_preserved_reading is accurate. If ceremonial: the reading is aspirational, and the actual constraint structure is closer to judicial authority despite the formal shell of parliamentary override.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliament_last_word_mythic_status, empirical, 'Whether Parliament''s section 4 override is substantive or ceremonial').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_rights_act_1998__incorporation_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_rights_act_1998__incorporation_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(huma_tr_t5, human_rights_act_1998__incorporation_reading, theater_ratio, 5, 0.44).
narrative_ontology:measurement(huma_tr_t10, human_rights_act_1998__incorporation_reading, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_rights_act_1998__incorporation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(huma_be_t5, human_rights_act_1998__incorporation_reading, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(huma_be_t10, human_rights_act_1998__incorporation_reading, base_extractiveness, 10, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_rights_act_1998__incorporation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(human_rights_act_1998__incorporation_reading, human_rights_act_1998__judicial_power_grab_reading).
narrative_ontology:affects_constraint(human_rights_act_1998__incorporation_reading, human_rights_act_1998__parliamentary_sovereignty_preserved_reading).

% DUAL FORMULATION NOTE:
% The HRA kernel admits three structurally distinct readings with different extractiveness values. The incorporation_reading (this story) treats the HRA as coordinative (ε≈0.28, Rope). The judicial_power_grab_reading treats it as extractive to Parliament (ε≈0.55, Tangled Rope or Snare). The parliamentary_sovereignty_preserved_reading treats it as retaining Parliamentary authority (ε≈0.35, Rope with institutional safeguard). Each reading generates a separate constraint story because the ε values and beneficiary/victim structures differ. They are linked via network.affects_constraints because they contend for the same kernel—determining which reading is empirically supported requires evidence about section 3 interpretation scope (does it rewrite or merely interpret?) and section 4 override frequency (is the power real or ceremonial?). The three stories form a constraint family: the incorporation reading assumes certain empirical claims about section 3 and section 4 that the other readings contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
