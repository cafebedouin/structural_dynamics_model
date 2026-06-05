% ============================================================================
% CONSTRAINT STORY: parliamentary_supremacy_statutes__parliament_act_1949
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parliamentary_supremacy_statutes__parliament_act_1949, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: parliamentary_supremacy_statutes__parliament_act_1949
 *   human_readable: Parliament Act 1949: Bootstrap Reduction of Upper House Delay
 *   domain: political/constitutional/legislative
 *
 * SUMMARY:
 *   The Parliament Act 1949 presents a unique constitutional constraint: an
 *   Act of Parliament that uses its own authorizing procedure to suppress the
 *   procedural delay it creates. The Act reduced the House of Lords' power to
 *   delay legislation from two years (under the 1911 Act) to one year, using
 *   the 1911 Act's own mechanism — which could not itself be delayed by the
 *   Lords. This creates a structural bootstrap: the constraint (delay)
 *   becomes the instrument of its own suppression. The constraint is neither
 *   a pure coordination mechanism (legislative passage is coordinated, but
 *   the reduction of delay serves the governing majority's timetable) nor
 *   pure extraction (the mechanism serves a legitimate legislative function,
 *   but is deployed strategically to remove obstruction). It is a textbook
 *   tangled rope — genuine coordination function layered with asymmetric
 *   extraction and active enforcement through constitutional doctrine.
 *
 * KEY AGENTS:
 *   - Governing Majority (Commons): Institutional/arbitrage — benefits from delay reduction; experiences constraint as coordination (legitimate legislative acceleration)
 *   - House of Lords: Trapped/powerless — losing revision time; cannot block the 1911 procedure using the 1911 procedure; bears full extraction
 *   - Opposition and Minority Interests: Organized/constrained — face compressed timeline for amendment and public mobilization; benefit from coordination when in power but suffer extraction in opposition
 *   - Constitutional Doctrine: Institutional/analytical — parliamentary supremacy doctrine becomes the enforcer of suppression; the 'no binding successor' principle blocks Lords resistance
 *   - Analytical Observer: Analytical/analytical — reveals the false summit (mountain classification masks contingent institutional choice) and the procedural bootstrap logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parliamentary_supremacy_statutes__parliament_act_1949, 0.38).
domain_priors:suppression_score(parliamentary_supremacy_statutes__parliament_act_1949, 0.62).
domain_priors:theater_ratio(parliamentary_supremacy_statutes__parliament_act_1949, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parliamentary_supremacy_statutes__parliament_act_1949, extractiveness, 0.38).
narrative_ontology:constraint_metric(parliamentary_supremacy_statutes__parliament_act_1949, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(parliamentary_supremacy_statutes__parliament_act_1949, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parliamentary_supremacy_statutes__parliament_act_1949, tangled_rope).
narrative_ontology:human_readable(parliamentary_supremacy_statutes__parliament_act_1949, "Parliament Act 1949: Bootstrap Reduction of Upper House Delay").
narrative_ontology:topic_domain(parliamentary_supremacy_statutes__parliament_act_1949, "political/constitutional/legislative").

domain_priors:requires_active_enforcement(parliamentary_supremacy_statutes__parliament_act_1949).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(parliamentary_supremacy_statutes__parliament_act_1949, 'e08a7cb8-90b9-426f-83af-c8ccd71830fc').
narrative_ontology:cs_kernel_codification('e08a7cb8-90b9-426f-83af-c8ccd71830fc', formalized).
narrative_ontology:cs_authority_grounding('e08a7cb8-90b9-426f-83af-c8ccd71830fc', lineage).
narrative_ontology:cs_interpretation_layer_present('e08a7cb8-90b9-426f-83af-c8ccd71830fc').
narrative_ontology:cs_reading_relation('e08a7cb8-90b9-426f-83af-c8ccd71830fc', parliamentary_supremacy_statutes__acts_of_union, coexists_with).
narrative_ontology:cs_reading_relation('e08a7cb8-90b9-426f-83af-c8ccd71830fc', parliamentary_supremacy_statutes__parliament_act_1911, influences).
narrative_ontology:cs_reading_relation('e08a7cb8-90b9-426f-83af-c8ccd71830fc', parliamentary_supremacy_statutes__fixed_term_parliaments_act, coexists_with).
narrative_ontology:cs_axiom('e08a7cb8-90b9-426f-83af-c8ccd71830fc', foundational, parliament_not_bound_by_successor).
narrative_ontology:cs_axiom_status(parliament_not_bound_by_successor, holdable).
narrative_ontology:cs_axiom_grounding('e08a7cb8-90b9-426f-83af-c8ccd71830fc', parliament_not_bound_by_successor, conventional).
narrative_ontology:cs_axiom('e08a7cb8-90b9-426f-83af-c8ccd71830fc', foundational, procedural_bootstrap_legitimate).
narrative_ontology:cs_axiom_status(procedural_bootstrap_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('e08a7cb8-90b9-426f-83af-c8ccd71830fc', procedural_bootstrap_legitimate, conventional).
narrative_ontology:cs_created_at('e08a7cb8-90b9-426f-83af-c8ccd71830fc', '').
narrative_ontology:cs_kernel_id(parliamentary_supremacy_statutes__parliament_act_1949, parliamentary_supremacy_statutes).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parliamentary_supremacy_statutes__parliament_act_1949, governing_majority_commons).
narrative_ontology:constraint_victim(parliamentary_supremacy_statutes__parliament_act_1949, house_of_lords_revision_capacity).
narrative_ontology:constraint_victim(parliamentary_supremacy_statutes__parliament_act_1949, minority_parliamentary_interests).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HOUSE OF LORDS POST-1949 (SNARE) — The second chamber faces structured suppression of its delaying power. The delay mechanism itself becomes the instrument of suppression: the procedure used to reduce delay (invoking the 1911 Act) is the same procedure the Lords cannot block. Trapped in a constitutional framework where their objections to suppression of delay are addressed using the very tool being suppressed. Maximum experienced extraction.
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__parliament_act_1949, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GOVERNING MAJORITY/COMMONS (ROPE) — Experiences the constraint as pure coordination: accelerating legislative passage through the second chamber is the legitimate function. The governing majority has structural exit (dissolution, election, arbitrage to public will). Benefits from the delay reduction without coercive overhead from their perspective — they perceive the mechanism as coordination enabling, not extraction.
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__parliament_act_1949, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: OPPOSITION AND MINORITY INTERESTS (TANGLED ROPE) — Organized but constrained. The two-year delay (under 1911 Act) provided a revision window to mobilize amendments and public pressure. The one-year reduction (1949) compresses this window — genuine extraction from the revision timeline. Yet the constraint coordinates legislative passage for all parties, including minorities when they are in power. Suppression is high (one year is limited) but coordination function remains real.
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__parliament_act_1949, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL DOCTRINE (PITON) — The constraint's legitimacy rests on the doctrine of parliamentary supremacy, which states that Parliament can amend or repeal any prior Act. But the 1949 Act uses this doctrine performatively: it amends the 1911 Act using the 1911 Act's own procedure, creating a self-justifying circularity. The doctrine persists through institutional inertia despite the logical strain — the theater ratio is high because the constitutional logic is partly ritual (invoking precedent to suppress dissent rather than deriving legitimacy from substantive principle).
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__parliament_act_1949, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 5: PARLIAMENTARY SUPREMACY AS NATURAL LAW (MOUNTAIN) — A civilizational analytical view might treat parliamentary supremacy as a natural law of constitutional democracies: a sovereign legislature cannot bind its successors — any prior Act can be amended by current Parliament. From this view, the 1949 Act is merely applying an immutable constitutional principle. However, the structural data contradicts mountain classification: there is an identifiable beneficiary (the governing majority), the suppression is engineered rather than inherent, and the constraint is deployed strategically rather than discovered as natural law. This is a false summit — naturalizing what is a contingent institutional arrangement built on procedural bootstrap.
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__parliament_act_1949, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The analytical perspective reveals the structural bind: the 1949 Act uses the 1911 procedure to suppress the 1911 delay, relying on the principle that Parliament cannot bind its successors. But this principle is being invoked by the majority to suppress the minority's procedural delay capacity. There is genuine coordination (legislative passage), genuine extraction (delay suppression), and genuine institutional enforcement (constitutional doctrine blocking Lords resistance). The constraint is hybrid — coordination layered with strategic extraction.
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__parliament_act_1949, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parliamentary_supremacy_statutes__parliament_act_1949_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__parliament_act_1949, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__parliament_act_1949, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(parliamentary_supremacy_statutes__parliament_act_1949, TR),
    TR >= 0.70.

:- end_tests(parliamentary_supremacy_statutes__parliament_act_1949_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The 1949 Act reduces the Lords' delay capacity by 50% (two years to one year), which is substantial extraction. However, the extraction is not maximal (snare-level) because: (1) genuine legislative coordination remains the primary function, (2) the reduction is not coercive against the Commons but rather removes obstruction to their timetable, (3) there is no equivalent of a snare's 'trap' — opposition parties know they will eventually govern and the delay reduction becomes a standing rule applying to all majorities. The extractiveness is moderate because the beneficiary (current governing majority) is temporary and the suppression serves a structural function (legislative passage) not merely a partisan interest. Suppression (0.62): Moderate-high. The mechanism is strongly suppressive: the Lords cannot block the 1911 procedure using the 1911 procedure (constitutional circularity prevents escape). The delay window is compressed. Yet suppression is not maximal because: (1) the one-year delay remains a meaningful revision period, (2) the procedure is formalized and transparent (not hidden coercion), (3) minority parties understand the rule applies equally when they govern. Theater ratio (0.55): Moderate. The theatrical component is real: constitutional doctrine is invoked performatively to justify a partisan benefit (accelerating the Labour government's agenda). The 1911 procedure is invoked ceremonially rather than substantively (the procedure's legitimacy derives from precedent, not from re-examination of its justification). Yet theater is not dominant: the mechanism is straightforward and the logic, though self-referential, is coherent within the supremacy doctrine.
 *
 * PERSPECTIVAL GAP:
 *   The governing majority sees rope (pure coordination enabling legislative progress). The Lords see snare (trapped within the delay suppression mechanism). The organized opposition sees tangled rope (extraction in opposition, coordination in power). Constitutional doctrine sees piton (the supremacy principle persists through invocation, not because it solves anything new). The analytical observer at the civilizational scale risks mountain classification (parliamentary supremacy as natural law) but structural evidence of false summit is decisive: there is an identifiable beneficiary (current majority), the suppression is engineered rather than discovered, and the constraint is deployed strategically rather than as an immutable principle. The perspectival gap reveals that 'natural law' framing masks partisan institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   The governing majority (institutional/arbitrage) has low directionality (d ≈ 0.15) — they are the beneficiary and have structural exit options (election, appeal to public will, legislative creativity). Applied through f(d), this produces low or negative chi for them — they experience the constraint as coordination without extraction. The House of Lords (powerless/trapped) has high directionality (d ≈ 0.90) — they are victims and trapped within the constitutional framework. Applied through f(d), this produces maximum chi — they experience maximum extraction. Opposition and minorities (organized/constrained) have moderate directionality (d ≈ 0.55) — they bear extraction in opposition but benefit when governing, and they have some organizational exit capacity (mobilization, party platform). Applied through f(d), this produces moderate chi. The analytical observer (d ≈ 0.72, canonical fallback for analytical power) sees the entire structure: beneficiary identity, suppression mechanism, and procedural bootstrap.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing the false summit: the mountain classification (parliamentary supremacy as natural law) collapses when structural agents are identified. The governing majority is the beneficiary; the Lords are the victim; the suppression is active enforcement. Once beneficiaries and victims are named, the mountain dissolves into tangled rope — there is genuine coordination (legislative passage) and genuine extraction (delay reduction serving majority timetable). The mandatrophy is not 'which type is correct?' but 'whose reading of the constraint dominates?': the mountain reading (supremacy as natural law) naturalizes what is contingent institutional choice. The falsity of the mountain is revealed not by empirical refutation but by structural disclosure: naming beneficiaries exposes the reading as partisan, not impartial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supremacy_recursion_ambiguity,
    'Is the 1911 Act''s ''cannot bind successor'' principle a genuine constitutional law or a procedural convention that can be suspended or reinterpreted when majorities demand it?',
    'Historical comparison: do successor parliaments actually treat prior Acts as binding in practice, or do they selectively suspend or reinterpret constraints when politically convenient? Analysis of Acts that tried to entrench provisions and their fate.',
    'If genuinely binding: parliamentary supremacy is a mountain-like natural law; the 1949 Act''s logic is sound. If conventionally suspended: supremacy is a performative doctrine deployed strategically; the 1949 Act is extraction laundered through constitutional principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supremacy_recursion_ambiguity, conceptual, 'Whether parliamentary supremacy is binding law or contingent convention').

omega_variable(
    procedural_bootstrap_legitimacy,
    'Does the use of the 1911 Act''s own procedure to shorten the 1911 delay constitute legitimate procedural compliance or illegitimate procedural bootstrap?',
    'Examine the 1911 Act''s text for explicit provisions about using its own procedure to modify its own terms. Analyze Lords debate on the 1949 Act: did the Lords contest the bootstrap logic, and if so, what counter-principle did they invoke?',
    'If legitimate: the 1949 Act is a straightforward application of established procedure. If bootstrap: the constraint is self-justifying (uses the delay mechanism to suppress the delay mechanism), making the legitimacy omega-dependent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(procedural_bootstrap_legitimacy, empirical, 'Legitimacy of using 1911 procedure to modify 1911 terms').

omega_variable(
    beneficiary_identification_contingency,
    'Is the beneficiary the ''governing majority in 1949'' (Labour under Attlee) or ''any future governing majority'' (universal beneficiary)?',
    'Track whether Conservative governments (when they returned to power) used the one-year delay or attempted to extend it back to two years. Did they treat the 1949 reduction as binding precedent, or did they later reassert the two-year delay?',
    'If contingent to 1949 Labour: extraction was partisan and temporary. If universal: the suppression became a permanent constraint on all future majorities (including Labour''s successors), revealing the true beneficiary as the current moment''s majority decision structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_identification_contingency, empirical, 'Beneficiary scope: 1949 Labour or all future majorities').

omega_variable(
    alternative_revision_pathway_closure,
    'Did the reduction from two years to one year eliminate specific revision opportunities or only compress the timeline without changing substantive amendment outcomes?',
    'Case study: identify bills passed under 1911 Act (two-year delay) that underwent significant amendment during the two-year window. Estimate whether those amendments would have been possible in a one-year window. Compare legislative outcomes pre- and post-1949.',
    'If specific pathways closed: extraction is substantive (revision capacity eliminated). If only compressed: extraction is primarily coercive (time pressure without structural closure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_revision_pathway_closure, empirical, 'Whether one-year reduction closes substantive amendment pathways or only increases time pressure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parliamentary_supremacy_statutes__parliament_act_1949, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pa49_tr_t0, parliamentary_supremacy_statutes__parliament_act_1949, theater_ratio, 0, 0.4).
narrative_ontology:measurement(pa49_tr_t2, parliamentary_supremacy_statutes__parliament_act_1949, theater_ratio, 2, 0.48).
narrative_ontology:measurement(pa49_tr_t5, parliamentary_supremacy_statutes__parliament_act_1949, theater_ratio, 5, 0.55).

% Extraction over time
narrative_ontology:measurement(pa49_be_t0, parliamentary_supremacy_statutes__parliament_act_1949, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(pa49_be_t2, parliamentary_supremacy_statutes__parliament_act_1949, base_extractiveness, 2, 0.3).
narrative_ontology:measurement(pa49_be_t5, parliamentary_supremacy_statutes__parliament_act_1949, base_extractiveness, 5, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(pa49_su_t0, parliamentary_supremacy_statutes__parliament_act_1949, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(pa49_su_t2, parliamentary_supremacy_statutes__parliament_act_1949, suppression_requirement, 2, 0.58).
narrative_ontology:measurement(pa49_su_t5, parliamentary_supremacy_statutes__parliament_act_1949, suppression_requirement, 5, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parliamentary_supremacy_statutes__parliament_act_1949, enforcement_mechanism).
narrative_ontology:affects_constraint(parliamentary_supremacy_statutes__parliament_act_1949, parliament_act_1911).
narrative_ontology:affects_constraint(parliamentary_supremacy_statutes__parliament_act_1949, house_of_lords_legislative_capacity).
narrative_ontology:affects_constraint(parliamentary_supremacy_statutes__parliament_act_1949, uk_government_timetable_acceleration).

% DUAL FORMULATION NOTE:
% The Parliament Act 1949 is a reading of the parliamentary_supremacy_statutes kernel. Related constraints in the family: parliament_act_1911 (the baseline delay mechanism, ε ≈ 0.30, Tangled Rope — pure coordination with some structural asymmetry favoring Commons), acts_of_union (the foundational constitutional kernel, ε ≈ 0.08, Mountain — statutory union as immutable constitutional fact, though false summit candidate), fixed_term_parliaments_act (the supremacy round trip, ε ≈ 0.35, Rope — statutes can be repealed; supremacy asserted via repeal). The 1949 Act is downstream of 1911 and has higher extractiveness due to the bootstrap mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
