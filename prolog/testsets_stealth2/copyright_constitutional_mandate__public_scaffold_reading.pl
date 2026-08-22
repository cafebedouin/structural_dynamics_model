% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__public_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__public_scaffold_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: copyright_constitutional_mandate__public_scaffold_reading
 *   human_readable: Copyright as Temporary Monopoly Enriching the Public Domain (Public-Scaffold Reading)
 *   domain: legal/constitutional/economic
 *
 * SUMMARY:
 *   This story instantiates the public-scaffold reading of the constitutional
 *   copyright mandate: the exclusive right is a deliberately transitional
 *   instrument — a temporary monopoly granted so that creative works are
 *   produced at all, on the understanding that everything secured expires
 *   into a permanently growing public commons. On this construal the
 *   arrangement has parties and a coordination function but no victims: the
 *   interim pricing power is the agreed price of the bargain, and the
 *   constitutional limited-Times language functions as a built-in sunset. The
 *   claim (scaffold) and the metrics (modest extraction, rising drift
 *   pressure) are authored independently; the measurement the corpus exists
 *   to take is whether the operative regime honors the sunset-centered design
 *   this reading describes or has drifted toward the enclosure construal
 *   carried in the linked sibling files. Per the ε-invariance principle the
 *   colloquial label copyright decomposes into three structurally distinct
 *   constraint stories — this file authors only the scaffold construal. KEY
 *   AGENTS (by structural relationship): - working_authors: during-term
 *   beneficiary with secondary cost-bearing position (moderate/constrained) —
 *   receives the exclusive right as production incentive, surrenders the work
 *   at expiry - publishers_and_distributors: commercial beneficiary
 *   (institutional/mobile) — realizes the term's exclusive value through
 *   distribution economics and captures most commercial revenue -
 *   general_public_works_users: net beneficiary with secondary cost-bearing
 *   position (powerless/trapped) — pays term-time access premia, inherits the
 *   commons at expiry - future_generations: excluded seat (powerless/trapped,
 *   universal scope) — deepest residual claimant on the commons, absent from
 *   term-setting - congress_legislature: agenda setter (institutional/mobile)
 *   — sets and adjusts terms inside the constitutional sunset -
 *   federal_courts: agenda setter with analytical second seat
 *   (institutional/constrained) — enforces exclusivity and adjudicates the
 *   mandate's meaning - constitutional_scholars_and_economists: analytical
 *   observer — evaluates whether the bargain delivers its stated end
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__public_scaffold_reading, 0.28).
domain_priors:suppression_score(copyright_constitutional_mandate__public_scaffold_reading, 0.22).
domain_priors:theater_ratio(copyright_constitutional_mandate__public_scaffold_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__public_scaffold_reading, scaffold).
narrative_ontology:human_readable(copyright_constitutional_mandate__public_scaffold_reading, "Copyright as Temporary Monopoly Enriching the Public Domain (Public-Scaffold Reading)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__public_scaffold_reading, "legal/constitutional/economic").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:has_sunset_clause(copyright_constitutional_mandate__public_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__public_scaffold_reading, '76365513-720c-4233-9574-cd52b3f745e9').
narrative_ontology:cs_kernel_codification('76365513-720c-4233-9574-cd52b3f745e9', fixed_text).
narrative_ontology:cs_authority_grounding('76365513-720c-4233-9574-cd52b3f745e9', lineage).
narrative_ontology:cs_interpretation_layer_present('76365513-720c-4233-9574-cd52b3f745e9').
narrative_ontology:cs_reading_relation('76365513-720c-4233-9574-cd52b3f745e9', copyright_constitutional_mandate__corporate_enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('76365513-720c-4233-9574-cd52b3f745e9', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('76365513-720c-4233-9574-cd52b3f745e9', foundational, monopoly_subordinate_to_public_end).
narrative_ontology:cs_axiom_status(monopoly_subordinate_to_public_end, holdable).
narrative_ontology:cs_axiom_grounding('76365513-720c-4233-9574-cd52b3f745e9', monopoly_subordinate_to_public_end, instrumental).
narrative_ontology:cs_axiom('76365513-720c-4233-9574-cd52b3f745e9', foundational, limited_times_constitutional_sunset).
narrative_ontology:cs_axiom_status(limited_times_constitutional_sunset, holdable).
narrative_ontology:cs_axiom_grounding('76365513-720c-4233-9574-cd52b3f745e9', limited_times_constitutional_sunset, conventional).
narrative_ontology:cs_reference_frame('76365513-720c-4233-9574-cd52b3f745e9', constitutional_public_domain_bargain).
narrative_ontology:cs_drift_state('76365513-720c-4233-9574-cd52b3f745e9', contemporary_post_extension_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('76365513-720c-4233-9574-cd52b3f745e9', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, working_authors).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, publishers_and_distributors).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, general_public_works_users).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__public_scaffold_reading, working_authors).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__public_scaffold_reading, general_public_works_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and revises the copyright statutes: sets term durations, defines exceptions, and adjusts scope, always inside the constitutional requirement that the exclusive right last only a limited time. Receives lobbying from incumbent rights-holders and public-interest coalitions alike, and retains standing authority to shorten prospective terms or broaden exceptions.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, congress_legislature, agenda_setter,
    institutional, generational, mobile, national).

% Administers the arrangement day to day: adjudicate infringement claims, award damages, and police the boundary between protected expression and the commons. Also review the mandate's meaning itself when challenged, as in term-extension litigation, where the bench both enforces the exclusivity it is examining and shapes what the bargain is understood to require.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, federal_courts, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__public_scaffold_reading, federal_courts, observer).

% Receive the exclusive right as production incentive: it secures a recoupment window for work that would otherwise be copied freely on release. They simultaneously carry the bargain's other side, since the right terminates and the work passes to the commons. Individual authors cannot renegotiate the constitutional design, though they may voluntarily accelerate dedication through open licenses.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, working_authors, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__public_scaffold_reading, working_authors, payer).

% Operate the commercial side of the exclusive window: acquire rights, manufacture and distribute copies, license derivatives, and manage catalogs as assets with expiry dates. They capture the majority of commercial copyright revenue during terms through control of distribution channels, and can shift investment across titles, formats, and territories as terms approach expiry.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, publishers_and_distributors, beneficiary,
    institutional, biographical, mobile, global).

% Pay retail premiums and forgo copying, adaptation, and redistribution while a work is under term, drawing on a vast pre-existing public-domain reservoir, fair-use exceptions, and open-licensed material meanwhile. When terms lapse they receive the work outright. No member of the public can individually exit the legal regime; their participation is the demand side of the bargain.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, general_public_works_users, beneficiary,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__public_scaffold_reading, general_public_works_users, payer).

% Are the deepest residual claimants on the commons: every expiry enlarges their inheritance and every extension shrinks it. They take no part in term-setting today and have no seat in any negotiation; the size of what they receive is decided entirely by others.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, future_generations, excluded,
    powerless, civilizational, trapped, universal).

% Evaluate whether the arrangement delivers its stated end: whether incentive effects justify the access delay, whether terms track the bargain's design, and whether the commons is actually enriched. They hold no stake in the revenue flows and publish assessments the other seats cite or ignore.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, constitutional_scholars_and_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creative works are public goods: non-rival, cheap to copy, expensive to produce. Unrestricted copying under-produces them. The arrangement solves this by leasing a temporary exclusive right that lets producers recoup investment, with automatic conversion of the work into the common stock at term expiry.
% TRANSFER_FUNCTION: During the term, pricing power over copies, performances, and derivatives moves from the public to the rightsholder, commercially concentrated in publishers and distributors with the author receiving the contracted share. At expiry the entire work, together with everything built on it, moves irreversibly to the public.
% ABSENT_VOICES: Future generations, the largest residual claimants on the commons, are absent from every term-setting negotiation; so are unauthorized remixer communities and orphan-work audiences, whose interest in a broad, quickly accessible commons is represented only indirectly. Their objection would be that each extension taxes precisely the seat that cannot object.
% DISAPPEARANCE_RATIONALE: Term-based financing for books, music, film, and software collapses overnight: no recoupment window, no catalog annuities, and the entire protected back catalog enters the commons at once. Production reorganizes within years around patronage, commissions, public funding, and collective voluntary schemes — a different creative economy, not the same one minus a fee.
% FOUNDING_PROBLEM: Chronic underproduction and enclosure of learning and the arts: the Statute of Anne (1710) and the Framers' clause were built to get books and science written and diffused by replacing the stationers' perpetual monopoly with a short, conditional, expiring exclusive right.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration comes from outside the beneficiary set: the Federalist No. 43 states the public-progress purpose at founding; the economic public-goods literature independently confirms that creative goods face underproduction without temporary exclusivity; historical scholarship on the Statute of Anne documents the anti-monopoly design intent. Industry attestations of incentive necessity coincide with their interest and are not counted; the non-party sources agree the founding problem is real and unresolved.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__public_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__public_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__public_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__public_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__public_scaffold_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).
:- end_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low-to-moderate (0.28) because the exclusive right is bounded by a constitutional sunset and its burden falls on seats this reading treats as net beneficiaries. Suppression is authored low (0.22) and raw — it is a structural property, unscaled by power or scope, reflecting ordinary civil enforcement rather than exit-blocking coercion. Theater (0.20) is low: registration formalities are largely vestigial, but the core mechanism genuinely transmits value from term to commons. Accessibility collapse is moderate-low (0.35): a vast public-domain reservoir, fair-use doctrine, and open licensing leave real alternatives standing, which is what a transitional mechanism should look like. Resistance (0.35) reflects copyfight and reform activism aimed chiefly at term inflation rather than at the bargain itself.
 *   
 *   The temporal series share one grid (points 0 through 30, approximating 1990 to 2020). All three tracked metrics rise monotonically — no cyclical pattern, so no intermittent-reinforcement reading is offered. The suppression_requirement series is authored deliberately rather than left static: the interval contains the construction of a dedicated enforcement layer (anti-circumvention rules, statutory damages escalation, takedown regimes, beginning with the 1998 digital-era copyright legislation near t=8), so enforcement-capacity change is part of the traced dynamic, not noise.
 *   
 *   Receipt surface: interim exclusive value demonstrably concentrates in publishers_and_distributors through control of distribution channels, hence the named gain_flow. Restoring the reference-frame design (shorter prospective terms, broader exceptions) faces international minimum-term treaty commitments and entrenched incumbent portfolios, so fixing_cost is authored prohibitive relative to its benefit, notwithstanding that a single statute could technically enact it.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the legislature's seat the arrangement presents as self-limiting by design, adjustable at will inside the sunset. From the author's seat it is earned compensation with a scheduled surrender. From the distributor's seat it is a portfolio asset with expiry dates. From the public's seat it is a toll bridge with a guaranteed demolition date. From the excluded future seat it is only a promise whose size others negotiate. The engine computes each seat's classification from power, exit, and directionality; the authored claim does not adjudicate which experience is definitive.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (working_authors, publishers_and_distributors, general_public_works_users, future_generations) derive low directionality — the constraint subsidizes them by design. No victim group is declared because the reading identifies none: term-time costs land on the public and author seats, which carry secondary payer positions and sit nearer symmetry than full target. Most seats carry global scope, which modestly amplifies effective extraction in the engine's computation, but with base ε at 0.28 and suppression unscaled at 0.22 the amplified values remain in the low band. The absence of any full-target seat is the structural signature distinguishing this construal from the enclosure sibling, in which the public seat derives near-full-target directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying the arrangement as scaffold rather than rope keeps the transition constitutive: a rope is steady-state coordination, whereas this mechanism's justification is the scheduled transfer of its subject matter into the commons — the sunset is not an administrative detail but the point of the design. The classification also blocks the opposite mislabeling: treating the interim exclusivity as pure extraction ignores that its burden falls on net-beneficiary seats under a bargain with a guaranteed end. Mandatrophy risk concentrates in term-length drift; the founding problem (chronic underproduction of learning and the arts) remains live, so no resolved-mandatrophy flag is authored, but the delivery omega and the drift_state entry track whether the mandate decays into performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which construal of the copyright_constitutional_mandate kernel governs the operative regime: this public_scaffold_reading (transitional monopoly serving public-domain enrichment), corporate_enclosure_reading (copyright as property entitlement with near-maximal terms), or judicial_ambiguity_reading (term length as unreviewable legislative discretion)?',
    'Compare the operative regime''s behavior against each construal''s predictions: term-length trajectory, fair-use breadth, rate of usable public-domain inflow, and review intensity applied to term-extension legislation.',
    'If the enclosure construal is operative, identifiable victims appear (the public during extended terms) and effective extraction rises sharply; if the discretion construal is operative, the scaffold''s sunset survives only as political contingency rather than structural guarantee; if this scaffold construal is operative, the authored low-to-moderate profile stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this story is one of three readings of the constitutional copyright kernel; classification depends on which construal the operative regime instantiates.').

omega_variable(
    public_domain_delivery,
    'Does the arrangement actually deliver the enrichment it promises — do expiring works enter a usable, accessible public domain at the rate the bargain assumes?',
    'Measure public-domain inflow and downstream reuse: digitization coverage of expired works, clearance costs for orphan works, and derivative output volume from recently expired corpora.',
    'Systematic delivery failure converts the reading''s coordination account into partial cover for enclosure drift and would justify shifting seat-level classifications toward extraction; robust delivery confirms the transitional profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_domain_delivery, empirical, 'Whether the bargain''s promised commons enrichment is observably delivered.').

omega_variable(
    enforcement_ratchet_trajectory,
    'Will the measured rise in the suppression requirement continue, plateau, or reverse over coming legislative cycles?',
    'Track enforcement-infrastructure indicators across sessions and treaty negotiations: anti-circumvention scope, statutory damages levels, and expansion of notice-and-takedown machinery.',
    'A continuing ratchet pushes the arrangement past the transitional band toward enforced enclosure, shifting per-seat classifications upward; stabilization preserves the scaffold character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_ratchet_trajectory, empirical, 'Trajectory of the enforcement layer constructed during the interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__public_scaffold_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t0, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0, 0.09).
narrative_ontology:measurement(copy_tr_t6, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement(copy_tr_t12, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(copy_tr_t18, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 18, 0.16).
narrative_ontology:measurement(copy_tr_t24, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(copy_tr_t30, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(copy_be_t0, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 0, 0.17).
narrative_ontology:measurement(copy_be_t6, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 6, 0.19).
narrative_ontology:measurement(copy_be_t12, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 12, 0.22).
narrative_ontology:measurement(copy_be_t18, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 18, 0.25).
narrative_ontology:measurement(copy_be_t24, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 24, 0.27).
narrative_ontology:measurement(copy_be_t30, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 30, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t0, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(copy_su_t6, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 6, 0.13).
narrative_ontology:measurement(copy_su_t12, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 12, 0.16).
narrative_ontology:measurement(copy_su_t18, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 18, 0.19).
narrative_ontology:measurement(copy_su_t24, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 24, 0.21).
narrative_ontology:measurement(copy_su_t30, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 30, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__public_scaffold_reading, resource_allocation).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, judicial_ambiguity_reading).

% DUAL FORMULATION NOTE:
% The natural-language label copyright covers three structurally distinct arrangements depending on what the constitutional mandate is taken to institute. This file authors the public-scaffold construal (temporary monopoly resolving into commons; no declared victims; low-to-moderate ε). The corporate_enclosure_reading sibling instantiates a property regime with near-boundary terms and an extracted public; the judicial_ambiguity_reading sibling instantiates a deference doctrine whose sunset is politically contingent. Their ε values differ widely; measuring them through one story would violate ε-invariance, so the family is decomposed and linked here and via cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
