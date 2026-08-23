% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__judicial_ambiguity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__judicial_ambiguity_reading, []).

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
 *   constraint_id: copyright_constitutional_mandate__judicial_ambiguity_reading
 *   human_readable: Judicial Deference on Copyright Term Length (Rational-Basis Zone)
 *   domain: legal/constitutional/political_economy
 *
 * SUMMARY:
 *   This story instantiates the judicial_ambiguity_reading of the kernel
 *   copyright_constitutional_mandate: the operative arrangement is the
 *   judiciary's settled practice of treating copyright term length as a zone
 *   of legislative discretion, reviewable only at rational-basis strength
 *   (crystallized in Eldred v. Ashcroft, 2003, upholding the 1998
 *   term-extension act's twenty-year addition). Under this reading the
 *   Progress Clause's 'limited Times' language commits the length question to
 *   Congress; the Court checks only for perpetuity or transparent bad faith.
 *   The arrangement genuinely coordinates — it answers 'who decides' without
 *   forcing judges to adjudicate incentive policy — and it simultaneously
 *   enables asymmetric transfer: each exercise of the discretion converts
 *   roughly twenty additional years of would-be-common value to incumbent
 *   catalogs, and the doctrine guarantees those conversions stand. Claim and
 *   metrics are authored independently: the claim is tangled_rope; the
 *   metrics describe observed operation. Epsilon's referent is the deference
 *   arrangement itself, assessed by this reading's own lights — the reading
 *   holds deference constitutionally proper while conceding, in its own
 *   structural delta, that it permits scaffold-to-enclosure drift without
 *   invalidation; the moderate epsilon reflects that enabling role. Sibling
 *   readings are separate stories: the public_scaffold_reading authors the
 *   same clause as a public-good ceiling (higher epsilon on extensions,
 *   public-domain beneficiaries), and the corporate_enclosure_reading authors
 *   it as maximal property (limit language reduced to rhetoric). Family links
 *   run through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - congress_legislative_branch: primary beneficiary and agenda-setter (institutional/arbitrage) — writes and rewrites term lengths; the doctrine insulates its choices from judicial reversal
 *   - supreme_court_judiciary: administering seat with a dual position (institutional/constrained) — maintains the deference through its rulings while paying in foregone review capacity
 *   - incumbent_rights_holders: principal monetary beneficiary (powerful/arbitrage) — converts each extension into added exclusive-revenue years on existing catalogs
 *   - public_domain_future_users: diffuse target (powerless/trapped) — inherits each extension as twenty more locked years
 *   - derivative_creators_and_documentarians: target with partial exit (moderate/constrained) — pays clearance costs or abandons works
 *   - ip_law_scholars_and_public_interest_litigants: analytical observer (analytical/analytical) — litigated the challenge and tracks the text-practice gap
 *   - originalist_interpreters_of_progress_clause: excluded voice (moderate/identity_locked) — holds a competing reading with no enforcing institution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.55).
domain_priors:suppression_score(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.31).
domain_priors:theater_ratio(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__judicial_ambiguity_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__judicial_ambiguity_reading, "Judicial Deference on Copyright Term Length (Rational-Basis Zone)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__judicial_ambiguity_reading, "legal/constitutional/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__judicial_ambiguity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__judicial_ambiguity_reading, 'd7f4af24-77f1-4a4b-ab62-cea1d348a6eb').
narrative_ontology:cs_kernel_codification('d7f4af24-77f1-4a4b-ab62-cea1d348a6eb', fixed_text).
narrative_ontology:cs_authority_grounding('d7f4af24-77f1-4a4b-ab62-cea1d348a6eb', lineage).
narrative_ontology:cs_interpretation_layer_present('d7f4af24-77f1-4a4b-ab62-cea1d348a6eb').
narrative_ontology:cs_reading_relation('d7f4af24-77f1-4a4b-ab62-cea1d348a6eb', copyright_constitutional_mandate__corporate_enclosure_reading, influences).
narrative_ontology:cs_reading_relation('d7f4af24-77f1-4a4b-ab62-cea1d348a6eb', copyright_constitutional_mandate__public_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('d7f4af24-77f1-4a4b-ab62-cea1d348a6eb', foundational, term_length_is_legislative_policy_question).
narrative_ontology:cs_axiom_status(term_length_is_legislative_policy_question, holdable).
narrative_ontology:cs_axiom_grounding('d7f4af24-77f1-4a4b-ab62-cea1d348a6eb', term_length_is_legislative_policy_question, conventional).
narrative_ontology:cs_axiom('d7f4af24-77f1-4a4b-ab62-cea1d348a6eb', foundational, rational_basis_suffices_for_limited_times).
narrative_ontology:cs_axiom_status(rational_basis_suffices_for_limited_times, holdable).
narrative_ontology:cs_axiom_grounding('d7f4af24-77f1-4a4b-ab62-cea1d348a6eb', rational_basis_suffices_for_limited_times, empirically_contingent).
narrative_ontology:cs_axiom('d7f4af24-77f1-4a4b-ab62-cea1d348a6eb', secondary, judicial_review_of_terms_exceeds_institutional_competence).
narrative_ontology:cs_axiom_status(judicial_review_of_terms_exceeds_institutional_competence, holdable).
narrative_ontology:cs_axiom_grounding('d7f4af24-77f1-4a4b-ab62-cea1d348a6eb', judicial_review_of_terms_exceeds_institutional_competence, conventional).
narrative_ontology:cs_reference_frame('d7f4af24-77f1-4a4b-ab62-cea1d348a6eb', legislative_discretion_framework).
narrative_ontology:cs_drift_state('d7f4af24-77f1-4a4b-ab62-cea1d348a6eb', contemporary_serial_extension_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d7f4af24-77f1-4a4b-ab62-cea1d348a6eb', '2026-08-06T15:04:12Z').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, congress_legislative_branch).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, incumbent_rights_holders).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_future_users).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, derivative_creators_and_documentarians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, supreme_court_judiciary).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, judicial_restraint_doctrine).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, legislative_supremacy_in_ip_policy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes copyright statutes and sets term lengths; passed the 1998 act adding twenty years to most existing and future terms. Because courts review such choices only for a stated rational purpose, its term decisions face no substantive judicial second-guessing. Collects expanded authority over intellectual property policy and political support from the industries that seek extensions.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, congress_legislative_branch, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__judicial_ambiguity_reading, congress_legislative_branch, beneficiary).

% Decides whether term extensions survive constitutional challenge. In the 2003 ruling it held that a stated legislative purpose suffices to satisfy the 'limited Times' language, articulating Congress's incentive and international-harmonization rationales without independently testing them. Maintains this posture through precedent; two justices dissented, showing the alternative remains reachable at institutional cost. Gives up review capacity each time it reaffirms the posture.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, supreme_court_judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__judicial_ambiguity_reading, supreme_court_judiciary, payer).

% Media conglomerates, music publishers, estates, and studios with deep back catalogs. Organize and fund the campaigns behind each term extension; when one passes, every work in their catalogs gains roughly twenty additional years of exclusive commercial use. Works already created gain the most, since no new creative decision responds to the added years. The judicial posture guarantees these legislative wins cannot be undone in court.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, incumbent_rights_holders, beneficiary,
    powerful, biographical, arbitrage, global).

% Readers, teachers, students, researchers, and hobbyists who would inherit works as they age into the public domain. Each extension keeps works that would have freed up locked for another twenty years. They have no collective organization, no seat in the legislative process, and no alternative route to the works; the cost arrives silently, spread across everyone born after each extension passes.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_future_users, payer,
    powerless, generational, trapped, national).

% Filmmakers, documentarians, novelists, musicians, and archivists who want to build on twentieth-century works. They can license at rising prices, navigate orphan-work uncertainty, or drop the project; clearance friction is the practical price they pay for works staying locked. Some organize through libraries and archives, giving them somewhat more voice than the diffuse public.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, derivative_creators_and_documentarians, payer,
    moderate, biographical, constrained, national).

% Law professors, public-interest lawyers, and archivist-led coalitions. They mounted the constitutional challenge decided in 2003 and a follow-on suit, and continue publishing critiques of the incentive rationale and the widening gap between the clause's text and accumulated practice. They neither collect nor pay under the arrangement; their seat is analytical and archival.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, ip_law_scholars_and_public_interest_litigants, observer,
    analytical, generational, analytical, national).

% Text-focused constitutional scholars and some jurists who read 'limited Times' as a substantive limit that courts should measure. Their reading lost the 2003 ruling and holds no enforcing institution; they persist in dissent, scholarship, and occasional concurrences. Leaving their method for the deference posture would mean abandoning their interpretive identity, so they remain outside the operating consensus.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, originalist_interpreters_of_progress_clause, excluded,
    moderate, generational, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__judicial_ambiguity_reading, incumbent_rights_holders).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__judicial_ambiguity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns the copyright-term question to the branch with electoral accountability and empirical access: Congress decides term length; courts confine themselves to checking for perpetuity or transparent bad faith. This spares the judiciary from making incentive-policy judgments it lacks data and mandate for, and gives rights holders and users a single predictable venue where terms change.
% TRANSFER_FUNCTION: Moves interpretive authority from the judiciary to Congress; and, each time Congress exercises the discretion, moves roughly twenty additional years of exclusive-commercial-use value out of the would-be public domain and to incumbent rights holders — a transfer the doctrine guarantees will stand.
% ABSENT_VOICES: Future generations of users — the people who would inherit each extended work — have no seat and no proxy in the process; the public domain has no institutional representative. Originalist interpreters of the Progress Clause hold a live competing reading but command no enforcing institution; their objection is recorded in dissent and scholarship, outside the decision loop.
% DISAPPEARANCE_RATIONALE: If courts tomorrow applied meaningful scrutiny to 'limited Times,' every past extension would become legally vulnerable, the lobbying economy around term legislation would reprice overnight, incumbents' catalog valuations built on assumed renewals would fall, and Congress would face a real constitutional constraint on further extension — the intellectual-property political economy would reorganize around judicially enforceable limits.
% FOUNDING_PROBLEM: Once Congress began lengthening terms repeatedly (interim extensions from 1962, the 1976 act overhaul, the 1998 twenty-year addition), courts faced challenges asking what 'limited Times' actually forbids. Judges lacked any principled metric for how short is short enough and did not want to adjudicate incentive-policy questions; the deference practice was built to answer 'who decides' — answer: Congress, subject only to a showing of rational purpose.
% FOUNDING_PROBLEM_CORROBORATION: Dissenting justices in the 2003 ruling attest from inside the judiciary that the interpretive question is manageable and that deference now functions as a rubber stamp; academic scholarship and the library/archive coalition corroborate that the founding problem is resolved and the practice now shields rent transfer. Congressional sponsors and the Copyright Office attest the problem remains live. Corroboration exists on both sides from seats outside the direct beneficiary set — hence contested.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__judicial_ambiguity_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__judicial_ambiguity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__judicial_ambiguity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metrics are authored for the arrangement's observed operation on a single shared grid (t=0 corresponds to 1993, t=5 to the 1998 term-extension enactment, t=10 to the 2003 Eldred ruling, t=30 to 2023). Base extractiveness climbs from 0.28 to 0.55: the doctrine's direct footprint is small, but each exercise of the discretion it protects converts twenty more years of would-be-common value to incumbent catalogs, and the accumulated stock of converted years grows with every cycle. Theater ratio jumps at t=10 (0.30 to 0.52) when review crystallizes into articulate-a-rationale-without-testing-it: the Court recites incentive and harmonization purposes it does not investigate, and the share of review activity that is functional (checking for outright perpetuity) shrinks relative to performative recitation. Suppression follows a rise-and-normalize arc rather than a monotonic ratchet: enforcement effort peaked at t=10 (0.44) when an organized coalition of archivists, scholars, and publishers forced the Court to defend the posture head-on, then decayed (0.31 by t=30) as the loss deterred further challenges — the arrangement now runs on stored precedent rather than active force. Accessibility collapse sits at 0.60: once the posture is understood, the judicial route is closed to challengers (two failed suits ended the litigation line), while the legislative route remains formally open to everyone and practically open to funded incumbents. Resistance at 0.35 reflects post-2003 courtroom dormancy with persistent scholarly critique. Claim and metrics are independent: the tangled_rope claim asserts genuine coordination plus asymmetric transfer; the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently. From the Congress seat the arrangement is legitimate democratic discretion — a beneficiary-side classification with low effective burden. From the incumbent seat it is a guaranteed investment climate: benefit without burden, arbitrage-grade exit. From the diffuse public seat the same arrangement computes as a locked, unrepresented transfer — target-side with amplified effective burden (trapped exit, powerless power, generational horizon). The Court seat is genuinely dual: it administers the posture (agenda-setting behavior) while paying for it in review capacity and doctrinal capital, and its constrained exit (precedent, stare decisis) keeps it from the beneficiary pole despite its administrative role. Identity-lock appears on the excluded originalist seat: the commitment to textual fixity is constitutive of that interpretive identity, so the seat cannot exit into the operating consensus without ceasing to be what it is; if that identity frame broke, the excluded voice would convert into ordinary opposition and the appearance of interpretive settlement would weaken.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: congress_legislative_branch (receives authority and insulation) and incumbent_rights_holders (receive the monetized output of each extension). Victim declarations: public_domain_future_users and derivative_creators_and_documentarians (bear the locked years and clearance costs). Derivation should place incumbents nearest the beneficiary pole (full benefit, arbitrage exit), Congress near it (subsidized, though it shares the institutional power atom with the Court), public users nearest the target pole (trapped, powerless, generational horizon), and derivative creators somewhat less far (constrained exit, partial organization through library and archive coalitions — the closest thing to coalition power in the victim set, historically insufficient to move national term policy). No directionality overrides are authored: the available override granularity is per power atom, and Congress and the Court share the institutional atom, so an override calibrated for the Court's dual position would misstate Congress. The Court's mixed position is left to structural derivation plus this commentary.
 *
 * MANDATROPHY ANALYSIS:
 *   Mislabeling risks run both ways. Reading the arrangement as pure coordination would miss the compounding transfer: the discretion is real, but its exercise is systematically purchased, and the doctrine removes the last institutional brake. Reading it as pure extraction would miss the genuine interpretive problem — someone must decide what 'limited' means, judges lack a principled metric and a data mandate, and delegation to the accountable branch is a defensible answer. The tangled-rope classification holds both facts. On mandatrophy: the founding problem (who decides term length) is contested rather than dead — defenders attest it is live, critics attest it is solved and the practice now functions as a rubber stamp. Because the verdict is contested rather than dead-plus-rearranging, the zombie-capture flag should not fire; the honest state is a mandate half-overtaken by its own success at removing friction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates only the judicial_ambiguity_reading of the kernel copyright_constitutional_mandate. What would change structurally if a sibling reading were adopted instead?',
    'Classify the sibling stories (public_scaffold_reading, corporate_enclosure_reading) on their own structural data and compare seat-by-seat classifications across the family.',
    'Under the public_scaffold_reading, term ceilings become judicially enforceable and public-domain interests enter as beneficiaries, raising measured extraction on each extension; under the corporate_enclosure_reading, the limit language loses even rhetorical force and the arrangement drifts toward pure enclosure. This story''s classification holds only for the deference reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel membership: one of three readings of the copyright constitutional mandate.').

omega_variable(
    constitutional_fixity_victim_status,
    'The reading''s structural delta names ''constitutional fixity'' as the injured party, but a principle is not an actor — which concrete classes actually bear the cost of lost fixity?',
    'Trace welfare losses from each extension to named classes (future users, derivative creators, archives); if losses resist actor-based accounting, discount the actor-based extraction accordingly.',
    'If the injury is genuinely actorless, the victims entries overstate effective extraction and the arrangement sits closer to a neutral delegation than the metrics suggest; if traceable, the actor-based accounting stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_fixity_victim_status, conceptual, 'Whether the deepest injury (erosion of constitutional fixity) maps onto identifiable cost-bearing actors.').

omega_variable(
    incentive_rationale_empirical_status,
    'Do serial term extensions actually produce additional creative output, or do they mostly transfer revenue on already-existing works?',
    'Economic analysis of the marginal incentive effect of term length (creation responds to the present value of early years; distant-year value is negligible), plus comparison of output trends before and after major extensions.',
    'If the incentive effect is negligible, the rational basis the Court articulates is empty, the theater ratio understates performativity, and the reading''s instrumental axiom faces evidentiary foreclosure pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_rationale_empirical_status, empirical, 'Empirical status of the incentive rationale that carries the deference.').

omega_variable(
    de_facto_perpetuity_threshold,
    'Is there a cumulative point at which serial extensions amount to a de facto perpetual term that even the deference reading must strike down?',
    'Model the cumulative term trajectory under repeated extension; test whether expected remaining term at creation approaches perpetuity, and whether the Court''s own anti-perpetuity floor remains coherent.',
    'If a threshold exists and is crossed, the deference posture becomes internally unstable — the reading must either abandon deference or concede the arrangement violates its own floor, shifting classification toward pure extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(de_facto_perpetuity_threshold, conceptual, 'Whether serial extension eventually defeats the ''limited'' qualifier on the reading''s own terms.').

omega_variable(
    legislative_capture_degree,
    'Does Congress exercise the discretion the doctrine protects autonomously, or does it operate as a transmission belt for concentrated rights-holder preferences?',
    'Correlate extension votes with lobbying and campaign finance records; compare the deliberation quality of term bills against ordinary copyright legislation.',
    'High capture means the judicial check was removed from the branch most exposed to capture, amplifying effective burden on public seats; low capture supports the genuine-discretion framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_capture_degree, empirical, 'Degree to which the discretion-being-deferred-to is itself captured.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__judicial_ambiguity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t0, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(copy_tr_t5, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(copy_tr_t10, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement(copy_tr_t15, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 15, 0.56).
narrative_ontology:measurement(copy_tr_t20, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(copy_tr_t25, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 25, 0.6).
narrative_ontology:measurement(copy_tr_t30, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(copy_be_t0, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(copy_be_t5, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 5, 0.34).
narrative_ontology:measurement(copy_be_t10, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(copy_be_t15, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 15, 0.47).
narrative_ontology:measurement(copy_be_t20, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(copy_be_t25, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 25, 0.53).
narrative_ontology:measurement(copy_be_t30, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 30, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t0, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(copy_su_t5, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(copy_su_t10, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(copy_su_t15, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(copy_su_t20, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(copy_su_t25, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 25, 0.33).
narrative_ontology:measurement(copy_su_t30, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 30, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__judicial_ambiguity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__public_scaffold_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'copyright's constitutional mandate' decomposes into three structurally distinct claims per the epsilon-invariance principle: this file (judicial deference on term length — moderate epsilon, enabling role), the corporate_enclosure_reading (maximalist property reading — limit language as rhetoric), and the public_scaffold_reading (public-good ceiling — extensions presumptively illegitimate). Each carries its own epsilon, beneficiaries, and victims. Structurally this reading is the load-bearing middle: it is what lets the enclosure reading advance incrementally without constitutional confrontation, and what strips the scaffold reading of judicial traction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
