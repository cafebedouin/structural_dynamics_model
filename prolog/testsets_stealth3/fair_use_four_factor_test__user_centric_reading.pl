% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__user_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__user_centric_reading, []).

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
 *   constraint_id: fair_use_four_factor_test__user_centric_reading
 *   human_readable: Fair Use as Affirmative User Right (User-Centric Reading of the Four-Factor Test)
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This story instantiates the user-centric reading of the fair use
 *   four-factor test: fair use as an affirmative right of users to make
 *   unauthorized uses of copyrighted works, with the four statutory factors
 *   weighed to preserve public access and cultural production. On this
 *   reading the operating arrangement transfers use-value from rights holders
 *   to the public without payment, and that transfer is largely the designed
 *   price of the constitutional bargain rather than wrongful taking — hence
 *   low epsilon, with the residual 0.28 counting only the excess beyond what
 *   this reading's own weighing would charge for (wholesale substitutional
 *   uses and uncompensated commercial ingestion at scales the access
 *   rationale does not reach). The colloquial label 'fair use' decomposes
 *   into three structurally distinct constraints, one per reading of the
 *   shared kernel; this file authors only the user-centric instantiation, and
 *   the epsilon differences across the family are documented in
 *   kernel_context and the network note. KEY AGENTS (by structural
 *   relationship): - public_and_educational_users: primary beneficiary
 *   (moderate/constrained) — act without license inside the envelope -
 *   secondary_creators: beneficiary (moderate/constrained) — parodists,
 *   documentarians, biographers, remixers - libraries_archives_museums:
 *   organized beneficiary (organized/constrained) — preservation and access
 *   infrastructure - platform_intermediaries: scaled beneficiary
 *   (powerful/arbitrage) — monetize sheltered uses at industrial scale -
 *   copyright_rights_holders: primary payer (institutional/mobile) —
 *   surrender fees and gating control - independent_working_creators:
 *   voiceless payer (powerless/trapped) — absorb losses with no seat in the
 *   arenas where the envelope is drawn - federal_courts: agenda setter
 *   (institutional/constrained) — weigh the factors case by case -
 *   congress_ip_committees: dormant agenda setter (institutional/mobile) —
 *   holds the statutory text, has not moved it in decades - ip_law_scholars:
 *   analytical observer (analytical/analytical) — supplies the arguments
 *   every seat borrows
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__user_centric_reading, 0.28).
domain_priors:suppression_score(fair_use_four_factor_test__user_centric_reading, 0.52).
domain_priors:theater_ratio(fair_use_four_factor_test__user_centric_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__user_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__user_centric_reading, "Fair Use as Affirmative User Right (User-Centric Reading of the Four-Factor Test)").
narrative_ontology:topic_domain(fair_use_four_factor_test__user_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__user_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__user_centric_reading, 'da1e9ef2-a0e4-48d6-bc80-5f50bee1d4ca').
narrative_ontology:cs_kernel_codification('da1e9ef2-a0e4-48d6-bc80-5f50bee1d4ca', fixed_text).
narrative_ontology:cs_authority_grounding('da1e9ef2-a0e4-48d6-bc80-5f50bee1d4ca', lineage).
narrative_ontology:cs_interpretation_layer_present('da1e9ef2-a0e4-48d6-bc80-5f50bee1d4ca').
narrative_ontology:cs_reading_relation('da1e9ef2-a0e4-48d6-bc80-5f50bee1d4ca', fair_use_four_factor_test__creator_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('da1e9ef2-a0e4-48d6-bc80-5f50bee1d4ca', fair_use_four_factor_test__transformative_use_reading, coexists_with).
narrative_ontology:cs_axiom('da1e9ef2-a0e4-48d6-bc80-5f50bee1d4ca', foundational, fair_use_is_affirmative_user_right).
narrative_ontology:cs_axiom_status(fair_use_is_affirmative_user_right, holdable).
narrative_ontology:cs_axiom_grounding('da1e9ef2-a0e4-48d6-bc80-5f50bee1d4ca', fair_use_is_affirmative_user_right, deontological).
narrative_ontology:cs_axiom('da1e9ef2-a0e4-48d6-bc80-5f50bee1d4ca', foundational, access_preservation_outweighs_owner_compensation).
narrative_ontology:cs_axiom_status(access_preservation_outweighs_owner_compensation, holdable).
narrative_ontology:cs_axiom_grounding('da1e9ef2-a0e4-48d6-bc80-5f50bee1d4ca', access_preservation_outweighs_owner_compensation, instrumental).
narrative_ontology:cs_reference_frame('da1e9ef2-a0e4-48d6-bc80-5f50bee1d4ca', public_access_preserving_user_right).
narrative_ontology:cs_drift_state('da1e9ef2-a0e4-48d6-bc80-5f50bee1d4ca', contemporary_ai_litigation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('da1e9ef2-a0e4-48d6-bc80-5f50bee1d4ca', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, public_and_educational_users).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, secondary_creators).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, libraries_archives_museums).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, platform_intermediaries).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, copyright_rights_holders).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, independent_working_creators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, copyright_rights_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Read, quote, copy, and reuse published works for study, teaching, commentary, and personal enrichment without seeking permission or paying license fees. When a use falls inside the fair use envelope they act without negotiating; outside it they must buy access or do without. Their practical alternative to the doctrine is a licensing market most of them cannot afford to navigate.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, public_and_educational_users, beneficiary,
    moderate, biographical, constrained, national).

% Make parodies, documentaries, biographies, critical essays, and remix works that incorporate existing material. Their productions depend on quoting, excerpting, or transforming sources; permission-based clearance for every incorporation would often be refused outright or priced beyond the project's budget. They gain working room from the doctrine and spend real effort documenting their reliance on it against challenge.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, secondary_creators, beneficiary,
    moderate, biographical, constrained, national).

% Preserve, lend, digitize, and provide research access to collections containing vast quantities of in-copyright material. Institutional practice leans on the doctrine for preservation copies, interlibrary loan, and text-mining pilots. Their associations litigate and lobby continuously to hold the envelope open, and they have no substitute collection to withdraw to if it closes.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, libraries_archives_museums, beneficiary,
    organized, generational, constrained, national).

% Run search indexes, video hosting, code repositories, and machine-learning training pipelines that ingest copyrighted works at industrial scale. Court decisions treating indexing, transformation, and analysis as fair use underwrite core product features worth billions annually. They maintain legal teams to defend the envelope and engineering teams to design within it, and they can shift operations across jurisdictions if the envelope narrows.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, platform_intermediaries, beneficiary,
    powerful, generational, arbitrage, global).

% Own catalogs of books, films, music, and software and sell licenses for reproduction, adaptation, and distribution. Uses sheltered by the doctrine proceed without payment and without their consent, reducing both licensing revenue and control over how their works circulate. They respond with edge-testing litigation, technological protection measures, and restrictive contract terms, while continuing to license everything the doctrine leaves them. Industry groups argue the long-run health of the cultural commons their tolerance feeds eventually returns value to them; the direct annual flow runs the other way.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, copyright_rights_holders, payer,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__user_centric_reading, copyright_rights_holders, beneficiary).

% Writers, illustrators, musicians, and photojournalists who live on per-use income from their catalogs. When their work is quoted, reposted, or ingested without payment they absorb the loss individually, with no seat in the trade associations, platform negotiations, or test-case litigation where the doctrine's boundaries get argued. Their recourse is a cease-and-desist letter they usually cannot afford to enforce.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, independent_working_creators, payer,
    powerless, biographical, trapped, national).

% Decide which unauthorized uses stand by weighing the four statutory factors case by case. Each opinion redraws the envelope's edges; appellate panels and the Supreme Court periodically consolidate the lines. They cannot decline the balancing task or delegate it, and their institutional self-understanding ties them to stewarding the constitutional bargain between incentive and access.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, federal_courts, agenda_setter,
    institutional, generational, constrained, national).

% Hold the statutory text that codifies the four factors and could widen, narrow, or abolish the doctrine by amendment. Committee calendars, campaign finance from content industries, and user-side advocacy all press on the choice; recent decades show maintenance rather than revision, leaving the envelope's motion to the courts.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, congress_ip_committees, agenda_setter,
    institutional, generational, mobile, national).

% Map the doctrine's operation, audit its outcomes, and supply the arguments every seat borrows. They publish competing accounts of what the four factors are for, and their disagreements travel into briefs and opinions. They hold no vote and collect no fee; their stake is the coherence of the account.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, ip_law_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__user_centric_reading, platform_intermediaries).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__user_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pre-authorizes a defined class of unauthorized uses — criticism, comment, news reporting, teaching, scholarship, research, and the transformations courts accept — so that culturally valuable uses which could never survive individual rights clearance proceed by default. It solves the clearing gridlock that universal licensing would create: negotiation costs, owner holdouts, unlocatable rights holders, and orphan works would otherwise stop most quotation, preservation, indexing, and follow-on creation before it starts.
% TRANSFER_FUNCTION: Moves use-value of existing works from rights holders to everyone else who touches them: readers, teachers, parodists, archivists, and platforms take reproduction, adaptation, and analysis without payment, while rights holders surrender the license fees and the gating control those uses would otherwise command. Decision-rights over a defined band of uses move from owners to users.
% ABSENT_VOICES: Independent working creators — the writers, illustrators, and musicians whose per-use income erodes — have no seat in the arenas where the envelope is drawn: not in the trade associations that litigate for owners, not in the platform legal teams that defend users, and rarely in the test cases that set precedent. They would argue for compensated-use middle paths (extended collective licensing, resale-style royalties) that neither incumbent camp currently champions.
% DISAPPEARANCE_RATIONALE: Overnight repeal would force every quotation, parody, classroom excerpt, archive scan, search index, and training run into a licensing negotiation. Transaction costs and owner holdouts would stop most of them; platforms would strip features built on the envelope or relocate; libraries would lock down digital collections; secondary creation would thin out. Prices, product designs, and the shape of the cultural record would all rearrange around a universal-clearance regime.
% FOUNDING_PROBLEM: Reconcile the exclusive-rights grant with its constitutional purpose. English and American copyright always paired the bookseller's monopoly with privileged unlicensed uses (fair abridgment, translation, criticism), and Folsom v. Marsh in 1841 codified Justice Story's factors to keep exclusive rights from hardening into de facto thought monopolies that starve criticism, learning, and follow-on creation.
% FOUNDING_PROBLEM_CORROBORATION: Courts across roughly 180 years restate the balancing problem in every leading opinion (Folsom, Sony, Campbell, Google v. Oracle), including in terms favorable to owners; historical scholarship on copyright's origins (Patterson and Lindberg and successors) attests the access side of the original statutory bargain; and owner-side filings concede the public-interest purpose exists while disputing where the balance sits. Attestation therefore comes substantially from outside the user constituency — with the caveat that courts sit ambiguously, as stewards of the doctrine as well as witnesses to its problem.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__user_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__user_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__user_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_four_factor_test__user_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__user_centric_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__user_centric_reading_tests).
:- end_tests(fair_use_four_factor_test__user_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28) because the referent is the standing four-factor regime assessed by this reading's own lights: the uncompensated transfer it shelters is the bargain's price, and only the overbroad residue (substitutional copying, uncompensated industrial ingestion) counts as extraction. Suppression is authored higher (0.52) and is deliberately NOT reconciled to extractiveness: suppression is a raw structural property, unscaled by directionality or scope, and it tracks the coercive force that maintains forced tolerance — owners cannot opt out of the envelope, face litigation exposure when they police it (good-faith takedown duties, fee awards against overaggressive enforcement), and cannot fully contract around the doctrine, even though they retain rich business-model alternatives. Theater is low (0.24): the factor weighing does real sorting work under this reading. Accessibility collapse is blended (0.45): the owner's exclusion option collapses almost completely inside the envelope, while licensing alternatives persist outside it and users retain paid channels. Resistance is high (0.70): the envelope meets permanent industry counter-mobilization — test-case litigation, technological protection measures, contract terms, and international treaty pressure. The claimed type (tangled_rope) is stated from structure — a genuine coordination function (clearing-gridlock resolution), an identified payer set bearing costs through the same structure, and adversarial enforcement — independently of the metric values; the engine computes per-seat types from the data. The measurement series share one time grid (all three metrics at all ten points). The series oscillate mildly rather than drifting monotonically: dips at 1991 (pre-consolidation lull) and 2023 (the market-harm revival in Warhol) mark expansion-backlash-recalibration cycles driven by industry counter-mobilization; the oscillation is a side effect of that cycle with an intermittent-reinforcement flavor (each expansion invites chilling litigation that raises assertion costs for smaller users), not noise. Base properties are measured at the interval end (2026), in the post-Warhol recalibration phase.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different lived types from identical doctrine. From the payer seat (rights holders), the arrangement is loss-shaped: compulsory tolerance of uncompensated use, enforced against them. From the beneficiary seats it is gift-shaped: an entitlement exercised without negotiation. From the agenda-setter seat (courts) it is neither — a standing balancing mandate they cannot decline. Same-level divergence is sharpest between platform_intermediaries and copyright_rights_holders, who hold comparable institutional-scale power yet opposite directionalities: the platforms hold arbitrage-grade exit (jurisdiction shopping, feature redesign) while owners hold mobile-but-lossy exit (business-model adaptation around a shrinking control perimeter). A quieter identity-lock operates on the judiciary: the bench's institutional self-conception as steward of the Progress Clause bargain binds it to the balancing task regardless of which reading of the kernel it credits, so the interpretive layer absorbs drift without surfacing revision.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the user-side seats toward the beneficiary end of directionality: public users, secondary creators, libraries, and platforms all receive uncompensated use-value, with platforms the largest absolute recipients (industrial-scale sheltered ingestion) despite sitting furthest from the reading's imagined prototypical user. Victim declarations drive both rights_holder seats toward the target end: institutional owners bear forgone fees and surrendered gating control (with a contested long-run commons offset noted via their secondary beneficiary position), while independent working creators bear the same erosion with no offsetting benefit and no seat. Courts and congressional committees occupy near-symmetric administrative positions — they collect nothing and pay nothing directly. No directionality overrides are authored: the role-plus-exit data already separates the seats, and overrides are keyed by power atom, so any correction aimed at the institutional owners would smear onto the courts and committees that share the institutional atom. Scope notes: the doctrine's spatial scope is national (a US statutory and case-law structure), while the owners and platforms it binds operate globally — the mismatch between national rule and global footprint is part of why verification of the envelope's edges is contentious.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping the exclusive-rights grant from hardening into a thought monopoly that starves criticism, learning, and follow-on creation — is live, so nothing here is atrophied and no mandatrophy is declared. The classification discipline guards against two opposite mislabels. Reading the low epsilon as rope-certainty would ignore the identified payer set and the adversarial enforcement machinery the envelope requires; hence tangled_rope. Reading the payer set as a snare signature would ignore that the coordination function is the doctrine's core purpose rather than cover for it. The drift risk to watch is piton-ward: if automated micro-licensing ever made universal clearance cheap, the factor weighing could persist as ritual over a solved problem — theater_ratio would climb past 0.5 while the founding problem quietly died. The temporal series exist to catch exactly that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the four-factor test''s normative structure an affirmative user right weighed for public access (this reading), a narrow property exception weighed for creator incentives (creator_centric_reading), or a transformativeness screen with market harm subordinated for meaning-adding uses (transformative_use_reading)?',
    'Doctrinal-trajectory analysis of Supreme Court factor treatment across Sony, Campbell, Google v. Oracle, and Warhol, combined with constitutional-purpose scholarship; a sustained access-first weighing pattern with explicit user-right language would confirm this reading''s structure.',
    'Creator-centric confirmation flips the victim set to users and raises epsilon on unauthorized use sharply, inverting this story''s low-epsilon profile; transformative confirmation relocates the contest to factor-one dominance and leaves the entitlement''s normative status unresolved.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the shared four-factor kernel correctly states its structure.').

omega_variable(
    rights_holder_net_position,
    'Are rights holders net losers from the fair use envelope, or do commons feedback effects (criticism driving discovery, follow-on creation feeding future catalogs, tolerance legitimating the statutory bargain) return at least as much value as the uncompensated uses take?',
    'Market studies of licensing behavior adjacent to fair use zones: documentary errors-and-omissions insurance costs after fair use best practices, permissions-market volume trends, and text-and-data-mining license uptake where fair use status is uncertain.',
    'Net-loss confirmation firms the extraction component and pushes the structure toward the snare boundary at the margin; net-benefit confirmation supports rope certification and validates the reading''s own long-run-benefit defense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_holder_net_position, empirical, 'Whether the payer seat is net-negative over the long run.').

omega_variable(
    ai_scale_boundary_stress,
    'Does the access-preserving justification extend to industrial-scale ingestion of copyrighted works (LLM training, corpus-wide text and data mining), or does scale convert the user right into a subsidy that breaks the bargain it was weighed to protect?',
    'Outcomes of pending training-data litigation together with market-replacement evidence: whether model outputs substitute for the ingested works'' licensing markets or complement them.',
    'Extension at scale keeps epsilon low while multiplying the transferred volume; rejection narrows the envelope, splits the beneficiary set, and raises measured extraction on the newly excluded class of uses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ai_scale_boundary_stress, empirical, 'Whether the user right survives industrial-scale ingestion.').

omega_variable(
    kernel_codification_framing,
    'Is the stabilized kernel the statutory text of section 107 (fixed_text, with the judiciary as interpretive layer), or the accumulated judicial practice itself (practice-grounded, with no fixed textual kernel)?',
    'Compare how the doctrine behaves when text and practice diverge: whether factor treatment tracks the statute''s enumerated purposes or case-law glosses the text never mentions, and whether proposed statutory amendments track practice or reset it.',
    'Adopting the practice-framing would move authority_grounding to practice or distributed, remove the fixed-text anchor, re-date the kernel''s origin to Folsom v. Marsh rather than 1976, and change how drift_state reads.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_codification_framing, conceptual, 'Alternative framings of what the stabilized kernel is.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__user_centric_reading, 1976, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_four_factor_test__user_centric_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(fair_tr_t1984, fair_use_four_factor_test__user_centric_reading, theater_ratio, 1984, 0.12).
narrative_ontology:measurement(fair_tr_t1991, fair_use_four_factor_test__user_centric_reading, theater_ratio, 1991, 0.11).
narrative_ontology:measurement(fair_tr_t1994, fair_use_four_factor_test__user_centric_reading, theater_ratio, 1994, 0.14).
narrative_ontology:measurement(fair_tr_t2003, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2003, 0.16).
narrative_ontology:measurement(fair_tr_t2012, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2012, 0.18).
narrative_ontology:measurement(fair_tr_t2015, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(fair_tr_t2021, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2021, 0.21).
narrative_ontology:measurement(fair_tr_t2023, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2023, 0.24).
narrative_ontology:measurement(fair_tr_t2026, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2026, 0.24).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1976, 0.1).
narrative_ontology:measurement(fair_be_t1984, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1984, 0.14).
narrative_ontology:measurement(fair_be_t1991, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1991, 0.13).
narrative_ontology:measurement(fair_be_t1994, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1994, 0.17).
narrative_ontology:measurement(fair_be_t2003, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2003, 0.2).
narrative_ontology:measurement(fair_be_t2012, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2012, 0.23).
narrative_ontology:measurement(fair_be_t2015, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2015, 0.26).
narrative_ontology:measurement(fair_be_t2021, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2021, 0.28).
narrative_ontology:measurement(fair_be_t2023, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2023, 0.26).
narrative_ontology:measurement(fair_be_t2026, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2026, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 1976, 0.22).
narrative_ontology:measurement(fair_su_t1984, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 1984, 0.3).
narrative_ontology:measurement(fair_su_t1991, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 1991, 0.28).
narrative_ontology:measurement(fair_su_t1994, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 1994, 0.34).
narrative_ontology:measurement(fair_su_t2003, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2003, 0.38).
narrative_ontology:measurement(fair_su_t2012, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2012, 0.44).
narrative_ontology:measurement(fair_su_t2015, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(fair_su_t2021, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2021, 0.54).
narrative_ontology:measurement(fair_su_t2023, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2023, 0.52).
narrative_ontology:measurement(fair_su_t2026, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2026, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__user_centric_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, transformative_use_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'fair use' decomposes into three structurally distinct constraints, one per reading of the shared four-factor kernel (constraint family; all members link one another via affects_constraints). This file is the user_centric_reading: low epsilon on unauthorized use, public and educational users as primary beneficiaries, rights holders as the payer set. creator_centric_reading authors the same statutory surface with the structure inverted — users as the constrained party, epsilon on unauthorized use high, creator incentives as the vindicated end. transformative_use_reading authors the weighing-axis variant: transformativeness dominates and market harm is subordinated for meaning-adding uses, without settling where the entitlement itself sits. Upstream/downstream: the creator-centric property frame historically grounds the exception's existence (an exception presupposes the property right), while this reading supplies the access rationale that transformative practice borrows as its normative fuel — downstream pressure without logical elimination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
