% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__absolutist_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: speech_protection_boundary__absolutist_reading
 *   human_readable: Absolutist Speech-Protection Boundary (Brandenburg Bright Line)
 *   domain: constitutional law/political philosophy
 *
 * SUMMARY:
 *   Within the contested speech_protection_boundary kernel, this story
 *   instantiates the absolutist_reading: a constitutional arrangement under
 *   which expressive protection is near-absolute and the unprotected set
 *   contains only advocacy directed to and likely to produce imminent lawless
 *   action (the Brandenburg line). The arrangement solves a real coordination
 *   problem — it strips the state of discretion to define 'harm' and thereby
 *   to prosecute unpopular advocacy — while its costs land asymmetrically:
 *   minoritized communities and individual harassment targets absorb
 *   aggregate dignitary and participatory harm that sibling readings would
 *   treat as actionable. The sibling readings (harm_limited_reading,
 *   balancing_reading) are separate constraints with their own epsilon,
 *   beneficiary/victim structures, and classifications, linked through
 *   network.affects_constraints; this file does not average over them or
 *   hedge epsilon across them. Claimed type and metrics are authored
 *   independently: I claim tangled_rope because the structure genuinely
 *   coordinates (a credible anti-censorship precommitment) while imposing
 *   asymmetric costs through the same bright line; the metrics describe
 *   moderate extraction with a rising platform-era trajectory.
 *
 * KEY AGENTS:
 *   - first_amendment_judiciary: Agenda setter (institutional/constrained) — administers the boundary, repels expansion attempts, collects nothing
 *   - political_dissidents: Primary intended beneficiary (moderate/mobile) — hold the unconditional protection guarantee
 *   - digital_platform_operators: Secondary beneficiary and commercial capturer (institutional/arbitrage) — monetize the widest protected set
 *   - minoritized_communities: Primary target class (organized/constrained) — bear aggregate harm as externality
 *   - targeted_harassment_victims: Concentrated targets (powerless/trapped) — remedy path foreclosed below the incitement line
 *   - equality_advocates: Excluded voice (organized/constrained) — argue for harm-conditioned boundaries from outside the adjudicative frame
 *   - comparative_speech_regime_scholars: Analytical observer (analytical/analytical) — see the full structure across jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, 0.48).
domain_priors:suppression_score(speech_protection_boundary__absolutist_reading, 0.6).
domain_priors:theater_ratio(speech_protection_boundary__absolutist_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__absolutist_reading, "Absolutist Speech-Protection Boundary (Brandenburg Bright Line)").
narrative_ontology:topic_domain(speech_protection_boundary__absolutist_reading, "constitutional law/political philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__absolutist_reading, '203a7216-7eb7-4dda-b6dc-f5d70706f18a').
narrative_ontology:cs_kernel_codification('203a7216-7eb7-4dda-b6dc-f5d70706f18a', fixed_text).
narrative_ontology:cs_authority_grounding('203a7216-7eb7-4dda-b6dc-f5d70706f18a', lineage).
narrative_ontology:cs_interpretation_layer_present('203a7216-7eb7-4dda-b6dc-f5d70706f18a').
narrative_ontology:cs_reading_relation('203a7216-7eb7-4dda-b6dc-f5d70706f18a', speech_protection_boundary__harm_limited_reading, forecloses).
narrative_ontology:cs_reading_relation('203a7216-7eb7-4dda-b6dc-f5d70706f18a', speech_protection_boundary__balancing_reading, forecloses).
narrative_ontology:cs_axiom('203a7216-7eb7-4dda-b6dc-f5d70706f18a', foundational, only_imminent_incitement_unprotected).
narrative_ontology:cs_axiom_status(only_imminent_incitement_unprotected, holdable).
narrative_ontology:cs_axiom_grounding('203a7216-7eb7-4dda-b6dc-f5d70706f18a', only_imminent_incitement_unprotected, deontological).
narrative_ontology:cs_axiom('203a7216-7eb7-4dda-b6dc-f5d70706f18a', foundational, state_discretion_is_primary_speech_danger).
narrative_ontology:cs_axiom_status(state_discretion_is_primary_speech_danger, holdable).
narrative_ontology:cs_axiom_grounding('203a7216-7eb7-4dda-b6dc-f5d70706f18a', state_discretion_is_primary_speech_danger, empirically_contingent).
narrative_ontology:cs_reference_frame('203a7216-7eb7-4dda-b6dc-f5d70706f18a', bright_line_precommitment_boundary).
narrative_ontology:cs_drift_state('203a7216-7eb7-4dda-b6dc-f5d70706f18a', contemporary_platform_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('203a7216-7eb7-4dda-b6dc-f5d70706f18a', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__absolutist_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, political_dissidents).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, digital_platform_operators).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, minoritized_communities).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, targeted_harassment_victims).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, marketplace_of_ideas_hypothesis).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, anti_censorship_precommitment_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts define and defend the boundary case by case: they decide which claims for protection or restraint succeed, and since 1969 they have consistently refused to widen the unprotected set beyond advocacy directed to imminent lawless action. They absorb political pressure each time they reject a harm-based restriction, and they collect no revenue from the arrangement; their stake is doctrinal custody, and precedent binds them to the line they inherited.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, first_amendment_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% People advocating unpopular causes — antiwar activists, labor organizers, radical critics of the majority — hold an unconditional guarantee that the state will not suppress their advocacy however offensive it is found. They rarely occupy the targeted position, so their exposure to the arrangement's costs is limited; the protection travels with whatever cause they take up, so leaving one arena changes nothing about what they receive.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, political_dissidents, beneficiary,
    moderate, biographical, mobile, national).

% Companies operating large speech platforms host everything short of direct incitement with minimal legal exposure, monetizing attention and engagement including inflammatory material. The narrow unprotected set keeps their mandatory removal obligations small; they can shift operations across jurisdictions when local rules tighten, and they fund litigation that defends the wide protected set.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, digital_platform_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Communities defined by race, religion, sexuality, or ethnicity absorb the aggregate effect of speech the boundary refuses to reach: slurs, dignity attacks, coordinated intimidation short of incitement, and a public sphere in which their equal standing is routinely contested. Civil-rights organizations give them collective voice, but they cannot leave the discourse environment, and the legal system offers no remedy for harm below the incitement line.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, minoritized_communities, payer,
    organized, generational, constrained, national).

% Individuals singled out for harassment campaigns — doxxing, pile-ons, threats framed as hyperbole — find that the conduct almost never constitutes incitement to imminent lawless action, so no legal remedy attaches. Leaving the platform or the public conversation is costly and often professionally or socially ruinous; the remedy-seeking path itself is closed by the boundary they live under.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, targeted_harassment_victims, payer,
    powerless, immediate, trapped, local).

% Movements arguing that dignity, equality, and freedom from harassment should condition protection hold no seat in the framework's adjudication: their proposals register only as attempted amendments or litigated challenges, which the boundary's custodians reject. They operate in legislatures, campuses, and international bodies rather than inside the doctrine.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, equality_advocates, excluded,
    organized, generational, constrained, national).

% Legal and political theorists compare this boundary with peer democracies that condition protection on demonstrated harm, documenting what each regime protects, what each pushes onto unprotected populations, and where the trajectories diverge. They collect nothing and pay nothing; their stake is descriptive.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, comparative_speech_regime_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__absolutist_reading, digital_platform_operators).
narrative_ontology:fixing_cost_class(speech_protection_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line, discretion-minimizing precommitment: speakers, platforms, and the state all know in advance exactly where protection ends, removing the case-by-case uncertainty that historically let governments define 'harm' expansively and prosecute unpopular advocacy.
% TRANSFER_FUNCTION: Moves legal protection — and the practical immunity that comes with it — to virtually all expressive acts, including harassing and dignity-attacking ones; moves the costs of those acts (intimidation, withdrawal from public discourse, aggregate dignitary harm) onto the individuals and communities targeted, who receive no compensating remedy.
% ABSENT_VOICES: Equality advocates and targets of aggregate harm would object that the boundary was drawn without weighing their costs; they stand outside the adjudicative frame, so their objections surface only as amendment attempts and rejected litigation rather than as inputs to where the line sits.
% DISAPPEARANCE_RATIONALE: If the bright line vanished overnight, speech regulation would reorganize immediately: governments would regain discretion to restrict on harm grounds, platforms would face liability pressure that reshapes what they host, and speakers would lose the advance guarantee that currently underwrites risky advocacy. Every named seat's situation depends on the line existing where it does.
% FOUNDING_PROBLEM: State suppression of political dissent under vague harm standards: the pre-1969 era of prosecuting communists, civil-rights agitators, and antiwar protesters whenever a majority or an administration deemed their advocacy dangerous to society.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians document the prosecutorial record the standard was built against, and comparative scholars corroborate that governments in peer democracies repeatedly exploit broad harm definitions to silence opposition — corroboration that comes substantially from outside the arrangement's benefiting parties. Equality advocates, though they dispute the line's width, concede the anti-censorship rationale it answers is real.
narrative_ontology:disappearance_verdict(speech_protection_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__absolutist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__absolutist_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) because the arrangement pairs a genuine, widely shared coordination good with a real asymmetric incidence: the same line that guarantees dissident speech guarantees harassing speech, and the resulting aggregate harm concentrates on identifiable communities. Suppression (0.60) records the machinery that holds the line: courts actively refuse harm-based claims, the amendment path is practically closed, and remedy-seeking below the incitement threshold is foreclosed by design — suppression here is structural (foreclosed remedies), not speaker-directed coercion, and it is authored as a raw structural property, unscaled by power or scope; only extractiveness gets context scaling in the engine. Theater is low (0.18): the boundary performs real protective work every day, though talismanic invocations of the doctrine have grown as its defense becomes more ritualized. Accessibility_collapse (0.52) reflects that alternative boundary regimes remain fully live in other jurisdictions and frameworks but collapse inside this one once the reading governs. Resistance (0.62) is high and persistent: every generation mounts a challenge wave — campus speech codes, harassment litigation, disinformation regulation — and each is repelled, sometimes reinforcing the doctrine it attacked. The measurement series run on one shared eight-point grid so both tracked metrics are authored at every examined time point; suppression_requirement is deliberately not tracked because enforcement capacity has been stable rather than the dynamic under study.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently, and the agenda-setter seat differently again. From the dissident seat the arrangement is a lifeline — pure protection, no bill. From the platform seat it is a commercial asset. From the minoritized-community and harassment-victim seats the same line operates as a closed courthouse door: the harm is real, the remedy is structurally unavailable, and exit from the discourse environment is not a live option. From the judiciary's seat the line is a trust to be kept against political weather, not a policy to be optimized. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Political dissidents sit near the beneficiary pole: they receive the guarantee and bear little of its cost, with mobile exit that never touches what they receive. Digital platform operators also derive low directionality from their beneficiary declaration, moderated slightly by their arbitration-grade mobility, which lets them harvest the wide protected set wherever it is widest. Minoritized communities carry high directionality: declared payers, constrained exit, generational exposure — the derivation places them near the full-target end, and their organized power dampens but does not reverse that placement. Targeted harassment victims sit nearest the full-target end of anyone: powerless, trapped, with the remedy path itself foreclosed. The judiciary appears in neither beneficiary nor victim lists; its derived directionality falls back toward symmetry, which matches its actual position as a steward that collects nothing and pays only in political capital spent defending the line.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Calling this a snare would erase the founding achievement: the bright line genuinely ended an era of discretionary political prosecution, and the anti-censorship mandate it serves is still live, not a zombie mandate — so the founding-problem interview shows no dead-mandate/world-rearranges mismatch. Calling it a rope would erase the incidence data: the costs are not spread evenly as a fair price of coordination but concentrate on communities with the least recourse, which is why the structure requires active enforcement to hold against fifty-plus years of expansion attempts. Tangled_rope holds both truths: coordination function and asymmetric payment through the same structure. The rising extractiveness trajectory tracks the platform era, when the protected set's externalized costs scaled with reach — accumulation layered onto a still-functioning coordination core, not decay of one into the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the speech_protection_boundary kernel — the absolutist_reading. What would the sibling readings (harm_limited_reading, balancing_reading) change structurally if instantiated instead?',
    'Author the sibling readings as separate constraint stories and compare their epsilon, victim sets, and classifications; the disagreement is located in the width and determination mechanism of the unprotected set.',
    'Under harm_limited_reading the actionable-harm set widens dramatically and the payer load shifts toward aggressive speakers; under balancing_reading enforcement discretion returns and the bright line dissolves into case judgment. Neither outcome changes this story''s epsilon — they are different constraints, not alternative measurements of this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: this story is the absolutist member of a three-reading kernel family.').

omega_variable(
    externality_or_constitutive_price,
    'Is the aggregate harm borne by minoritized communities an asymmetric imposition the boundary creates, or the constitutive price of universal protection that the reading knowingly accepts?',
    'Comparative analysis of jurisdictions with harm-conditioned boundaries: measure whether protected-class welfare and overall expressive participation improve enough to offset the reintroduced censorship risk; within this framework, trace whether harm incidence correlates with speaker-target identity asymmetry.',
    'If the incidence is asymmetric imposition, the tangled_rope reading hardens toward the snare end and remedial pressure is structurally justified; if it is a uniformly borne price of the coordination good, the arrangement sits nearer rope and the externality is an accepted coordination cost. This is the story''s central open question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_or_constitutive_price, conceptual, 'Whether the harm externality is extraction riding on coordination or the priced cost of the coordination itself.').

omega_variable(
    structural_vs_internalized_chilling,
    'Is the reduced participation of harassment targets structural (the legal remedy is foreclosed by the boundary) or internalized (self-silencing that would persist even where remedies exist)?',
    'Post-remedy trajectory comparison: measure participation rates in constitutionally identical spaces that differ in whether platform-level remedies exist; if withdrawal persists where remedies are available, the suppression is partly internalized.',
    'If largely internalized, the boundary''s measured suppression understates its full footprint and platform-level compensation structures become load-bearing for the arrangement''s stability; if structural, extending remedies below the incitement line would restore participation directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_chilling, empirical, 'Mechanism split for the chilling effect on targeted speakers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__absolutist_reading, 1969, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1969, speech_protection_boundary__absolutist_reading, theater_ratio, 1969, 0.1).
narrative_ontology:measurement_basis(spee_tr_t1969, observed).
narrative_ontology:measurement(spee_tr_t1978, speech_protection_boundary__absolutist_reading, theater_ratio, 1978, 0.11).
narrative_ontology:measurement_basis(spee_tr_t1978, observed).
narrative_ontology:measurement(spee_tr_t1987, speech_protection_boundary__absolutist_reading, theater_ratio, 1987, 0.12).
narrative_ontology:measurement_basis(spee_tr_t1987, observed).
narrative_ontology:measurement(spee_tr_t1996, speech_protection_boundary__absolutist_reading, theater_ratio, 1996, 0.14).
narrative_ontology:measurement_basis(spee_tr_t1996, observed).
narrative_ontology:measurement(spee_tr_t2005, speech_protection_boundary__absolutist_reading, theater_ratio, 2005, 0.16).
narrative_ontology:measurement_basis(spee_tr_t2005, observed).
narrative_ontology:measurement(spee_tr_t2014, speech_protection_boundary__absolutist_reading, theater_ratio, 2014, 0.18).
narrative_ontology:measurement_basis(spee_tr_t2014, observed).
narrative_ontology:measurement(spee_tr_t2020, speech_protection_boundary__absolutist_reading, theater_ratio, 2020, 0.19).
narrative_ontology:measurement_basis(spee_tr_t2020, observed).
narrative_ontology:measurement(spee_tr_t2026, speech_protection_boundary__absolutist_reading, theater_ratio, 2026, 0.2).
narrative_ontology:measurement_basis(spee_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t1969, speech_protection_boundary__absolutist_reading, base_extractiveness, 1969, 0.28).
narrative_ontology:measurement_basis(spee_be_t1969, observed).
narrative_ontology:measurement(spee_be_t1978, speech_protection_boundary__absolutist_reading, base_extractiveness, 1978, 0.31).
narrative_ontology:measurement_basis(spee_be_t1978, observed).
narrative_ontology:measurement(spee_be_t1987, speech_protection_boundary__absolutist_reading, base_extractiveness, 1987, 0.33).
narrative_ontology:measurement_basis(spee_be_t1987, observed).
narrative_ontology:measurement(spee_be_t1996, speech_protection_boundary__absolutist_reading, base_extractiveness, 1996, 0.36).
narrative_ontology:measurement_basis(spee_be_t1996, observed).
narrative_ontology:measurement(spee_be_t2005, speech_protection_boundary__absolutist_reading, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement_basis(spee_be_t2005, observed).
narrative_ontology:measurement(spee_be_t2014, speech_protection_boundary__absolutist_reading, base_extractiveness, 2014, 0.44).
narrative_ontology:measurement_basis(spee_be_t2014, observed).
narrative_ontology:measurement(spee_be_t2020, speech_protection_boundary__absolutist_reading, base_extractiveness, 2020, 0.47).
narrative_ontology:measurement_basis(spee_be_t2020, observed).
narrative_ontology:measurement(spee_be_t2026, speech_protection_boundary__absolutist_reading, base_extractiveness, 2026, 0.48).
narrative_ontology:measurement_basis(spee_be_t2026, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(speech_protection_boundary__absolutist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, speech_protection_boundary__harm_limited_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, speech_protection_boundary__balancing_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the free speech principle' conflates three structurally distinct boundary regimes. The absolutist reading (this file) fixes the boundary ex ante with a minimal unprotected set; the harm_limited_reading conditions protection on absence of significant dignitary/equality harm; the balancing_reading computes protection case-by-case. Each has its own epsilon, its own victim set, and its own classification; forcing them into one story would make epsilon observer-dependent, which the chi formula forbids. The upstream claim (the absolutist settlement of 1969) is cited as settled ground by both downstream contests, so this story links to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
