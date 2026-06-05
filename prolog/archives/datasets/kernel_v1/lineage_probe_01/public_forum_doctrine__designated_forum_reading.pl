% ============================================================================
% CONSTRAINT STORY: public_forum_doctrine__designated_forum_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_forum_doctrine__designated_forum_reading, []).

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
 *   constraint_id: public_forum_doctrine__designated_forum_reading
 *   human_readable: Public Forum Doctrine: Designated Forum Reading (Selective Generosity Constraint)
 *   domain: constitutional_law/first_amendment
 *
 * SUMMARY:
 *   The designated-forum reading of the public-forum doctrine instantiates a
 *   specific constitutional bargain: once a government decides to open a
 *   forum (a meeting room, a speaker's series, a grant program, a public
 *   plaza), it may not then curate speakers by viewpoint. The choice to open
 *   is discretionary; the mandate to be neutral once open is mandatory. This
 *   creates a tangled-rope dynamic: the doctrine coordinates speech access
 *   (once opened, speakers can rely on neutrality) while also suppressing
 *   cuatorial discretion (once opened, government loses selective
 *   generosity). The constraint appears as an immutable logical necessity to
 *   an observer at civilizational scale (surely constitutional equality
 *   requires neutrality once you open access), but structural analysis
 *   reveals it as a contested institutional choice — the
 *   government_speech_reading treats government expressive intent as primary
 *   (shifting the victim/beneficiary relationship), and the
 *   traditional_forum_reading claims streets and parks carry stronger
 *   protections by virtue of their immemorial use (shifting the question from
 *   'did government open this?' to 'what has this space always been used
 *   for?'). This reading presupposes that government discretion to exclude is
 *   the baseline, and that opening is voluntary — a presupposition the
 *   traditional_forum_reading directly rejects.
 *
 * KEY AGENTS:
 *   - Disfavored Speech Groups: Primary beneficiary (powerless/trapped) — gain access only when forum is opened and only under strict neutrality rules; cannot force opening but can police enforcement once open
 *   - Curating Administrators: Primary victim (institutional/constrained) — lose cuatorial discretion once forum is designated; cannot select viewpoints but must maintain the forum
 *   - Government Expressive Intent: Secondary victim (institutional/arbitrage) — government's own message-expression interest is subordinated to speaker access once forum is opened
 *   - Judicial System: Secondary beneficiary (institutional/arbitrage) — enforces clear, administrable neutrality rules; gains authority to police government curation
 *   - Advocacy Organizations: Mixed agent (powerful/mobile) — benefit from access and coordination rules but constrained by neutrality mandate in framing strategies
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the contingent institutional choice to prioritize speaker access over government curation discretion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_forum_doctrine__designated_forum_reading, 0.38).
domain_priors:suppression_score(public_forum_doctrine__designated_forum_reading, 0.5).
domain_priors:theater_ratio(public_forum_doctrine__designated_forum_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_forum_doctrine__designated_forum_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(public_forum_doctrine__designated_forum_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(public_forum_doctrine__designated_forum_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_forum_doctrine__designated_forum_reading, tangled_rope).
narrative_ontology:human_readable(public_forum_doctrine__designated_forum_reading, "Public Forum Doctrine: Designated Forum Reading (Selective Generosity Constraint)").
narrative_ontology:topic_domain(public_forum_doctrine__designated_forum_reading, "constitutional_law/first_amendment").

domain_priors:requires_active_enforcement(public_forum_doctrine__designated_forum_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_forum_doctrine__designated_forum_reading, '61938169-5419-40cb-8604-f17de226a646').
narrative_ontology:cs_kernel_codification('61938169-5419-40cb-8604-f17de226a646', formalized).
narrative_ontology:cs_authority_grounding('61938169-5419-40cb-8604-f17de226a646', lineage).
narrative_ontology:cs_interpretation_layer_present('61938169-5419-40cb-8604-f17de226a646').
narrative_ontology:cs_reading_relation('61938169-5419-40cb-8604-f17de226a646', public_forum_doctrine__government_speech_reading, coexists_with).
narrative_ontology:cs_reading_relation('61938169-5419-40cb-8604-f17de226a646', public_forum_doctrine__traditional_forum_reading, influences).
narrative_ontology:cs_axiom('61938169-5419-40cb-8604-f17de226a646', foundational, selective_generosity_forbidden).
narrative_ontology:cs_axiom_status(selective_generosity_forbidden, holdable).
narrative_ontology:cs_axiom_grounding('61938169-5419-40cb-8604-f17de226a646', selective_generosity_forbidden, deontological).
narrative_ontology:cs_axiom('61938169-5419-40cb-8604-f17de226a646', foundational, government_property_baseline_exclusion).
narrative_ontology:cs_axiom_status(government_property_baseline_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('61938169-5419-40cb-8604-f17de226a646', government_property_baseline_exclusion, deontological).
narrative_ontology:cs_reference_frame('61938169-5419-40cb-8604-f17de226a646', voluntary_forum_opening_with_mandatory_neutrality).
narrative_ontology:cs_drift_state('61938169-5419-40cb-8604-f17de226a646', contemporary_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('61938169-5419-40cb-8604-f17de226a646', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(public_forum_doctrine__designated_forum_reading, public_forum_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_forum_doctrine__designated_forum_reading, disfavored_speech_groups).
narrative_ontology:constraint_beneficiary(public_forum_doctrine__designated_forum_reading, marginalized_constituencies).
narrative_ontology:constraint_victim(public_forum_doctrine__designated_forum_reading, curating_administrators).
narrative_ontology:constraint_victim(public_forum_doctrine__designated_forum_reading, government_expressive_intent).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED SPEAKER (SNARE) — Once the forum is opened, the disfavored speaker experiences the designated-forum doctrine as an absolute mandate: neutrality is enforced, exclusion is forbidden. But the speaker had no control over whether the forum opened in the first place. The suppression is total: no alternative venue exists (the government controls the scarce resource), and the doctrine's neutrality requirement is not negotiable. Maximum extraction of structural powerlessness.
constraint_indexing:constraint_classification(public_forum_doctrine__designated_forum_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL RIGHTS ADVOCATE (TANGLED ROPE) — Gains access to opened forum but only under strict neutrality rules that constrain strategy (cannot argue the government has nefarious intent without risking viewpoint discrimination claims). Also benefits from the doctrine's coordination function: once opened, the forum becomes a common resource for all speakers. Mixed: real gain in access, but constrained by the asymmetric neutrality mandate that applies only to government curation.
constraint_indexing:constraint_classification(public_forum_doctrine__designated_forum_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: JUDICIAL SYSTEM (ROPE) — Enforces viewpoint neutrality as a coordination mechanism: once a forum is designated, the doctrine provides clear, administrable rules. Courts benefit from the bright-line rule (no viewpoint discrimination) because it eliminates case-by-case balancing and provides predictable outcomes. Extraction toward the judiciary is minimal — the doctrine gives courts authority to police forum curation, a net institutional gain.
constraint_indexing:constraint_classification(public_forum_doctrine__designated_forum_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: GOVERNMENT CURATOR (SNARE) — Once a forum is opened, the curator loses all discretion. The doctrine extracts from the curator's expressive intent: the curator wanted to host only compatible viewpoints but the neutrality mandate forbids selectivity. The curator cannot exit without closing the forum entirely (costly). The extraction is the suppression of cuatorial judgment in exchange for opening the door.
constraint_indexing:constraint_classification(public_forum_doctrine__designated_forum_reading, snare,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ADVOCACY ORGANIZATION (TANGLED ROPE) — Powerful actor that benefits from designated-forum status (gains access to government resources for organizing) and also experiences genuine coordination gain (the doctrine provides stable rules for when and how to access forums). Mobile: can organize alternative forums or shift tactics. But the doctrine also constrains strategy — neutrality requirements force the organization to frame arguments in universalist rather than faction-specific terms.
constraint_indexing:constraint_classification(public_forum_doctrine__designated_forum_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / DOCTRINAL INEVITABILITY (MOUNTAIN) — From a long-term and universal perspective, once a government opens a forum, some form of neutrality doctrine is logically inevitable: selective access to government-provided speech platforms is incompatible with equal-protection principles. The doctrine appears as a necessary logical consequence of the Constitution's neutrality requirement. However, the structural data reveals this as a false summit: the 'inevitable' framing naturalizes what is actually a contested institutional choice about whether to open forums at all and how broadly to read 'openness.'
constraint_indexing:constraint_classification(public_forum_doctrine__designated_forum_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_forum_doctrine__designated_forum_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(public_forum_doctrine__designated_forum_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(public_forum_doctrine__designated_forum_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(public_forum_doctrine__designated_forum_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint does create asymmetric costs and benefits. Curators lose discretion, speakers gain access. But the doctrine is not maximally extractive because it provides coordination value (once opened, speakers can rely on rules; administrators know the mandate). The measurement trajectory shows rising extractiveness over the interval as enforcement practice has tightened and courts have broadened what counts as a designated forum. Suppression (0.50): Moderate-high. Curators are suppressed from selective generosity (no alternatives to silence or neutral treatment). Speakers face suppression only if they are excluded outright (if included, they benefit). The suppression targets the curator's discretion, not the speaker's access. Theater ratio (0.35): Low-moderate. The doctrine is relatively functional — courts actually do enforce viewpoint neutrality, and the rules are administrable. The theater component enters when courts bend the doctrine to avoid its strictures (e.g., by declaring a forum 'limited' rather than 'designated,' or by accepting government's statement of forum purpose as binding). The measurement shows stable low theater because the doctrine's logic is transparent, even when courts stretch it.
 *
 * PERSPECTIVAL GAP:
 *   The designated-forum reading creates four distinct classifications from the same structural data. The curator sees a snare (loses all discretion once the forum is open, cannot exit without closing it entirely). The disfavored speaker sees a snare (has no control over whether the forum opens in the first place, can only demand enforcement once it does). The judicial system sees a rope (gains clear enforcement rules, benefits from the bright-line neutrality mandate). The powerful advocacy organization sees a tangled rope (gains access and coordination benefits but is constrained by neutrality requirements in framing arguments). The government's own expressive interest sees a snare (cannot use its own property to advance its message once it opens access). The analytical observer at civilizational scale risks seeing a mountain (neutrality is logically inevitable once you open access) — but the structural data reveals this as a false summit, because the designation-as-mandatory framing naturalizes a contingent choice about whether to open at all.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from beneficiary/victim position and exit options. Disfavored speakers start as powerless agents with no exit option (trapped) — they did not create the government property at issue and have no alternative forum available. They experience maximum extraction from the government's initial refusal to open. Once a forum is open, they experience partial de-extraction (they gain access) but also remain trapped by the neutrality rules (they cannot use government resources to argue against the government's curation policies). Curators are institutional agents who can theoretically exit by closing the forum, but closure is costly and legally exposed (courts have expanded the doctrine to make closures themselves subject to scrutiny). From the curator's position, the doctrine extracts cuatorial discretion. From the judicial system's position, the doctrine is a net benefit (bright-line rules, institutional authority, low transaction costs). The powerful advocacy organization has mobile exit options (can organize alternative forums, shift tactics, litigate) so their experience of extraction is mitigated by their capacity to respond.
 *
 * MANDATROPHY ANALYSIS:
 *   The designated-forum reading resolves the mandatrophy by distinguishing the choice to open (discretionary, not subject to the doctrine) from the mandate to be neutral once open (mandatory, strictly enforced). This avoids the incoherence of claiming the doctrine is both protective and extractive: it is protective of speakers (who benefit from neutrality guarantees) and extractive of government discretion (who loses cuatorial selectivity). The reading's internal logic is coherent, but it depends on a presupposition that the other readings contest: that government's baseline right is to exclude, and opening is the exception. If the traditional_forum_reading is correct (some forums are held in trust, and neutrality is not an exception but the underlying norm), then the designated-forum reading misidentifies what the constraint is extracting. If the government_speech_reading is correct (government has expressive interests that override speaker access in some contexts), then the doctrine's scope is narrower than this reading assumes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    designated_versus_limited_public_forum_boundary,
    'What distinguishes a designated public forum (subject to strict viewpoint neutrality) from a limited public forum (subject to viewpoint neutrality within the designated purpose)?',
    'Jurisprudential analysis of U.S. Supreme Court case law (Rosenberger, Good News Club, Walker v. Texas Division); examination of how courts apply the ''announced purpose'' test to distinguish categories.',
    'If the boundary is bright and stable: designated-forum doctrine provides reliable coordination. If the boundary is ambiguous or shifting: the doctrine provides cover for selective curation disguised as ''limited'' forum designation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(designated_versus_limited_public_forum_boundary, conceptual, 'Boundary between designated and limited public forums').

omega_variable(
    government_reopening_doctrine_scope,
    'Once a government opens a forum and then later restricts it, does the designated-forum doctrine continue to apply, or does government recover curating discretion?',
    'Examination of case law on government closure of previously open forums; analysis of whether closure is treated as reversal of prior designation or as content-based restriction within an ongoing forum.',
    'If closure is free: government can extract value by opening a forum temporarily to appear neutral, then closing it once undesired speakers have invested in access (trap-then-exclude). If closure is restricted: the designation is more durable and extraction is reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(government_reopening_doctrine_scope, empirical, 'Whether government can close reopened forums without triggering doctrine restrictions').

omega_variable(
    neutrality_mandate_versus_subsidy_logic,
    'Is the designated-forum doctrine''s neutrality mandate a free-speech protection or a subsidy-withdrawal mechanism in disguise?',
    'Doctrinal archaeology comparing designated-forum doctrine to unconstitutional-conditions doctrine; analysis of whether the ''must be neutral'' rule protects speech or extracts speech by conditioning forum access on viewpoint suppression.',
    'If protection: the doctrine reduces extraction and creates a common resource. If subsidy mechanism: the doctrine itself is the extraction tool — government opens the forum to gather all speech in one place, then enforces neutrality to prevent any speaker from capturing the resource.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_mandate_versus_subsidy_logic, conceptual, 'Whether neutrality mandate is protection or extraction mechanism').

omega_variable(
    reading_identity_natural_law_ambiguity,
    'Is this reading of the doctrine (once opened, must be neutral) a natural law of First Amendment logic, or a contested institutional choice that appears inevitable because beneficiaries have made it so?',
    'Historical analysis of pre-designated-forum-doctrine doctrine (did courts always require forum neutrality, or was this a 20th-century innovation?); comparative constitutional law (how do other democracies handle government-opened speech forums?); examination of alternative framings the government_speech_reading and traditional_forum_reading instantiate.',
    'If natural law: the mountain perspective is correct, and the doctrine''s constraints are immutable. If institutional choice: the mountain is a false summit, and the constraint is better understood as tangled_rope or snare depending on the speaker''s position.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_natural_law_ambiguity, conceptual, 'Whether designated-forum doctrine is logical necessity or contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_forum_doctrine__designated_forum_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pfd_tr_t0, public_forum_doctrine__designated_forum_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(pfd_tr_t3, public_forum_doctrine__designated_forum_reading, theater_ratio, 3, 0.31).
narrative_ontology:measurement(pfd_tr_t6, public_forum_doctrine__designated_forum_reading, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(pfd_be_t0, public_forum_doctrine__designated_forum_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(pfd_be_t3, public_forum_doctrine__designated_forum_reading, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(pfd_be_t6, public_forum_doctrine__designated_forum_reading, base_extractiveness, 6, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(pfd_su_t0, public_forum_doctrine__designated_forum_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(pfd_su_t3, public_forum_doctrine__designated_forum_reading, suppression_requirement, 3, 0.47).
narrative_ontology:measurement(pfd_su_t6, public_forum_doctrine__designated_forum_reading, suppression_requirement, 6, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_forum_doctrine__designated_forum_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(public_forum_doctrine__designated_forum_reading, public_forum_doctrine__government_speech_reading).
narrative_ontology:affects_constraint(public_forum_doctrine__designated_forum_reading, public_forum_doctrine__traditional_forum_reading).

% DUAL FORMULATION NOTE:
% The public_forum_doctrine kernel decomposes into three constraint stories, each instantiating a different reading. The designated_forum_reading (this story) treats government's baseline right as exclusion and opening as voluntary. The government_speech_reading treats government expression as primary and forum neutrality as inapplicable when government speaks. The traditional_forum_reading treats certain spaces (streets, parks) as held in trust with inherent neutrality requirements. These are not three observables of one constraint; they are three interpretively distinct claims about what the constitution requires. They affect each other: if the designated-forum reading is correct, then government_speech must be narrowly construed as an exception; if government_speech is broadly correct, then designated-forum neutrality is weakened; if traditional_forum is correct, then designated-forum is a subordinate category (traditional forums get stronger protection). All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
