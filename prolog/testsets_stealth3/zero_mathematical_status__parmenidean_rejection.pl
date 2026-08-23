% ============================================================================
% CONSTRAINT STORY: zero_mathematical_status__parmenidean_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_mathematical_status__parmenidean_rejection, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: zero_mathematical_status__parmenidean_rejection
 *   human_readable: Parmenidean Exclusion of Zero from the Number Domain
 *   domain: history of mathematics/philosophy of mathematics/conceptual history
 *
 * SUMMARY:
 *   The Parmenidean exclusion of zero is a standing arrangement that governed
 *   European mathematical and commercial practice for roughly two millennia:
 *   the null quantity is barred from the number domain, operations on it are
 *   undefined, and the bar is justified as ontological necessity — nothing
 *   cannot be, therefore no number can be nothing. The arrangement presents
 *   itself as a limit on what can coherently be thought, but it has
 *   identifiable beneficiaries (the abacist profession whose fee income
 *   depends on scarce computation, the scholastic faculties whose authority
 *   rests on adjudicating the number concept) and identifiable victims
 *   (merchants and astronomers who pay compounded arithmetic costs for its
 *   duration). KEY AGENTS (by structural relationship):
 *   scholastic_metaphysics_faculties — agenda-setter and beneficiary
 *   (institutional/identity_locked), administers the exclusion through
 *   curriculum and disputation; abacist_computation_guilds — primary
 *   beneficiary (organized/constrained), collects the computational-fee
 *   transfer; mediterranean_trade_merchants — primary target
 *   (organized/constrained), bears the arithmetic costs;
 *   observational_astronomers — target with partial relief
 *   (moderate/constrained), placeholder zero permitted, number-zero denied;
 *   hindu_arabic_algorists — excluded counterexample (organized/mobile),
 *   operational zero outside the conversation;
 *   florentine_mercantile_magistracy — enforcement arm
 *   (institutional/mobile), documentary bans; historians_of_mathematics —
 *   analytical observer (analytical/analytical). This file instantiates ONE
 *   reading of the zero_mathematical_status kernel; the sibling readings are
 *   separate constraints, not positions inside this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_mathematical_status__parmenidean_rejection, 0.63).
domain_priors:suppression_score(zero_mathematical_status__parmenidean_rejection, 0.74).
domain_priors:theater_ratio(zero_mathematical_status__parmenidean_rejection, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, extractiveness, 0.63).
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_mathematical_status__parmenidean_rejection, tangled_rope).
narrative_ontology:human_readable(zero_mathematical_status__parmenidean_rejection, "Parmenidean Exclusion of Zero from the Number Domain").
narrative_ontology:topic_domain(zero_mathematical_status__parmenidean_rejection, "history of mathematics/philosophy of mathematics/conceptual history").

domain_priors:requires_active_enforcement(zero_mathematical_status__parmenidean_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_mathematical_status__parmenidean_rejection, 'afa8786f-2aeb-4d34-9388-350e12f109df').
narrative_ontology:cs_kernel_codification('afa8786f-2aeb-4d34-9388-350e12f109df', fixed_text).
narrative_ontology:cs_authority_grounding('afa8786f-2aeb-4d34-9388-350e12f109df', lineage).
narrative_ontology:cs_interpretation_layer_present('afa8786f-2aeb-4d34-9388-350e12f109df').
narrative_ontology:cs_reading_relation('afa8786f-2aeb-4d34-9388-350e12f109df', zero_mathematical_status__number_reading, forecloses).
narrative_ontology:cs_reading_relation('afa8786f-2aeb-4d34-9388-350e12f109df', zero_mathematical_status__placeholder_reading, influences).
narrative_ontology:cs_axiom('afa8786f-2aeb-4d34-9388-350e12f109df', foundational, nonbeing_is_not_intelligible).
narrative_ontology:cs_axiom_status(nonbeing_is_not_intelligible, holdable).
narrative_ontology:cs_axiom_grounding('afa8786f-2aeb-4d34-9388-350e12f109df', nonbeing_is_not_intelligible, deontological).
narrative_ontology:cs_axiom('afa8786f-2aeb-4d34-9388-350e12f109df', secondary, numbers_count_only_beings).
narrative_ontology:cs_axiom_status(numbers_count_only_beings, holdable).
narrative_ontology:cs_axiom_grounding('afa8786f-2aeb-4d34-9388-350e12f109df', numbers_count_only_beings, deontological).
narrative_ontology:cs_reference_frame('afa8786f-2aeb-4d34-9388-350e12f109df', being_exhausts_number_domain).
narrative_ontology:cs_drift_state('afa8786f-2aeb-4d34-9388-350e12f109df', post_brahmagupta_transmission, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('afa8786f-2aeb-4d34-9388-350e12f109df', '').
narrative_ontology:cs_kernel_id(zero_mathematical_status__parmenidean_rejection, zero_mathematical_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_mathematical_status__parmenidean_rejection, abacist_computation_guilds).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__parmenidean_rejection, scholastic_metaphysics_faculties).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, mediterranean_trade_merchants).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, observational_astronomers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__parmenidean_rejection, observational_astronomers).
narrative_ontology:constraint_vindicates(zero_mathematical_status__parmenidean_rejection, parmenidean_nonbeing_principle).
narrative_ontology:constraint_vindicates(zero_mathematical_status__parmenidean_rejection, aristotelian_void_denial).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach that being and non-being exhaust what is, and examine candidates through disputation on that basis; treat talk of a quantity that is nothing as a category mistake. Set the arts curriculum, license who may transmit the number concept, and collect the epistemic authority that comes from adjudicating it. Abandoning the exclusion would dissolve the framework the faculties exist to hand on, so the position is not experienced as chosen.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, scholastic_metaphysics_faculties, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(zero_mathematical_status__parmenidean_rejection, scholastic_metaphysics_faculties, beneficiary).

% Sell calculation as a service: multiplication, division, and ledger reconciliation performed on abacus and Roman-numeral tally, skills gated by apprenticeship. A notation letting any clerk carry out the same operations on paper would erase the fee base, so guilds defend the numeral rules they were trained in and petitioned city magistrates to keep cipher-bearing records void. Exit means writing off the apprenticeship system and competing against their own former customers.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, abacist_computation_guilds, beneficiary,
    organized, generational, constrained, regional).

% Move goods across long distances and keep accounts that partners, banks, and courts will honor. Without positional notation, every multiplication and division is bought labor or slow tally-work, and errors compound across long ledgers. Many keep Hindu-Arabic figures in private working copies; where ordinances void cipher-bearing documents, the efficient form carries legal risk, so the choice is between paying for computation and risking the paper.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, mediterranean_trade_merchants, payer,
    organized, biographical, constrained, continental).

% Compute ephemerides, calendar corrections, and planetary tables — the heaviest routine arithmetic of the age. Sexagesimal degree notation gives them a sanctioned placeholder mark for empty columns, which relieves transcription but confers none of the operational powers denied to the null quantity; table-making remains slow and error-prone, and errors propagate into navigation and feast-day dating.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, observational_astronomers, payer,
    moderate, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(zero_mathematical_status__parmenidean_rejection, observational_astronomers, beneficiary).

% Work in the Indo-Islamic world where the null quantity has defined operations — addition, subtraction, multiplication, division with stated caveats — standardized since Brahmagupta. Their treatises travel into Latin through translation houses, but they hold no chair, no guild, and no hearing in the jurisdictions that police the number concept; their existence is the standing demonstration the exclusion must explain away.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, hindu_arabic_algorists, excluded,
    organized, generational, mobile, continental).

% Administer the Arte del Cambio rules of 1299 barring Arabic numerals and ciphers from account books, on the stated ground that a single stroke turns a cipher into another figure while Roman numerals resist quiet alteration. Enforcement means voiding offending documents and fining users; capacity strains as usage spreads through the very merchant houses the rules govern, and repeal is a council vote away.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, florentine_mercantile_magistracy, agenda_setter,
    institutional, biographical, mobile, local).

% Reconstruct the contest from ledgers, ordinances, and treatises: how long the exclusion held, what it cost in computational labor, and why adoption ran ahead of permission. Positioned outside every faction's commitments, they can read the fee stream, the doctrine, and the counterexample together.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, historians_of_mathematics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_mathematical_status__parmenidean_rejection, abacist_computation_guilds).
narrative_ontology:fixing_cost_class(zero_mathematical_status__parmenidean_rejection, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds the number concept to the domain of existing things, keeping arithmetic consistent with the ontological framework in which being and non-being exhaust reality; secondarily, restricts commercial records to numeral forms resistant to silent alteration, since a cipher is changed by one stroke while Roman numerals and abacus-checked tallies are not.
% TRANSFER_FUNCTION: Moves computational labor and notation efficiency away from merchants and astronomers toward the abacist profession, which collects fees for operations the excluded notation would make cheap; moves epistemic authority over what counts as a number to the metaphysical faculties; transfers the risk of arithmetic error onto those denied the efficient tools.
% ABSENT_VOICES: The Hindu-Arabic algorists — Brahmagupta's successors and the Islamic computists — hold a fully operational counterexample and are structurally outside the Latin conversation; working clerks and factors who bear the daily arithmetic costs have no seat in disputation halls or drafting councils.
% DISAPPEARANCE_RATIONALE: If the exclusion vanished overnight — the null quantity admitted as a number with defined operations — positional notation would spread through commercial and astronomical practice within a generation, abacist fee income would collapse, ledger formats and university curricula would rewrite, and the pace of European computational science would jump by decades.
% FOUNDING_PROBLEM: Parmenides' problem: thought and speech about what-is-not seem to grant it some being, threatening the intelligibility of the whole. The arrangement was built to protect the category system by barring nothing from the objects of quantity; the medieval enforcement layer added a second founding purpose, securing commercial records against cipher alteration.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: the algorist textual tradition (Brahmagupta, al-Khwarizmi, Fibonacci's Liber Abaci) attests the operations run without ontological catastrophe; commercial practice migrated wherever jurisdiction permitted; historians of mathematics attest that zero-using commercial centers controlled documentary fraud procedurally, dissolving the security rationale. No party inside the arrangement corroborates deadness — the faculties asserted the problem's liveness to the end.
narrative_ontology:disappearance_verdict(zero_mathematical_status__parmenidean_rejection, world_rearranges).
narrative_ontology:founding_problem_status(zero_mathematical_status__parmenidean_rejection, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_mathematical_status__parmenidean_rejection, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zero_mathematical_status__parmenidean_rejection, 'none', 1).
narrative_ontology:epsilon_provenance(zero_mathematical_status__parmenidean_rejection, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_mathematical_status__parmenidean_rejection_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zero_mathematical_status__parmenidean_rejection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zero_mathematical_status__parmenidean_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.63: the exclusion prices arithmetic — multiplication and division without positional notation cost orders of magnitude more labor, the price falls on everyone who computes at scale, and the fee stream it protects is concentrated. Suppression 0.74: persistence depends on active machinery — disputation gatekeeping, curriculum control, and documentary ordinances that void cipher-bearing records — not on participant preference; the alternative (algorism) exists and works but is criminalized or delegitimized rather than unavailable. Theater 0.34: the ontological argumentation was genuinely load-bearing early in the window, but a growing share of enforcement activity is ritual citation of transmitted authority defending the exclusion rather than engagement with the counterexample. Accessibility_collapse 0.66: within the accepted ontological framework, admitting zero collapses as an option almost completely — the framework cannot coherently hold it — but escape requires exiting the framework itself, which is costly and, in documentary law, illegal. Resistance 0.68: sustained, distributed, and ultimately victorious — merchant adoption, clandestine algorism, translation traffic. The interval runs 1250–1400 CE at five years per unit; the Florence ordinance of 1299 lands at t=10. All three metric series share one time grid (t = 0,5,10,15,20,25,30). suppression_requirement is authored because the story specifically tracks enforcement-capacity build-up (ordinance regimes, examination practice), not merely shifting extraction; the series plateaus after t=15 as enforcement saturates. Suppression is authored as raw structure — only extractiveness is scaled by directionality and scope downstream. claimed_type is authored as tangled_rope from the structure (genuine coordination content, asymmetric incidence, active enforcement); the metrics are authored independently as the arrangement's observed operation; the doctrine's own self-presentation is mountain-shaped, and that gap is the datum the corpus exists to take.
 *
 * PERSPECTIVAL GAP:
 *   From the faculty seat the arrangement is ontological stewardship — the protection of intelligibility itself, defended in good faith. From the merchant seat it is a levy on every column of figures. From the excluded algorist seat it is a refusal to look at a working counterexample. Same-level divergence: merchants and astronomers hold similar formal standing yet different effective positions — the astronomers' licensed placeholder mark dampens their exposure relative to merchants, whose every ledger bears the full ban. Institutional pair: the faculties collect authority and the guilds collect fees — same side, different currencies — and the guilds' constrained exit makes them the faction that lobbies hardest for continued enforcement, since their human capital is worthless under the rival notation.
 *
 * DIRECTIONALITY LOGIC:
 *   The faculties and the guilds derive near-beneficiary directionality (subsidized by the arrangement; low effective extraction). Merchants derive near-full-target directionality: they bear the transfer and their exit is closed by documentary legality — unilateral adoption risks void instruments. Astronomers derive high-but-not-full target directionality through their dual position: full costs on table-scale arithmetic, partial relief through sanctioned placeholder notation. The magistracy sits administratively near the beneficiary end while bearing almost none of the arrangement's costs. The algorists are excluded rather than seated: their structural role feeds suppression and accessibility_collapse (they are the suppressed alternative) rather than any seat's directionality. No directionality_overrides are authored: the beneficiary/victim declarations plus exit options already yield these placements, and the astronomers' dual position is expressed through secondary_role rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting the category system from the paradoxes of speaking about nothing, and later securing records against cipher alteration — is dead: operational zero proved coherent, fraud proved controllable procedurally, and the number_reading displaced this arrangement everywhere. The mismatch signature (founding_problem_status=dead combined with disappearance_verdict=world_rearranges) flags the retrospective zombie condition: the arrangement persisted for centuries past its live rationale, sustained by guild rents and curricular inertia. Classification discipline prevents two mislabelings: a pure-extraction reading would erase the genuine coordination content (framework coherence for those inside the ontology, real record-security value before procedural controls matured) that made the arrangement durable and sincerely held; a pure-coordination reading would erase the concentrated fee stream and the criminalized alternative. During its operative phase the arrangement was a hybrid hardening under an enforcement ratchet — the rising suppression series records the hardening — and its historical dissolution came not from internal reform but from the excluded counterexample winning by migration and print.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_delta,
    'This constraint is the parmenidean_rejection reading of kernel zero_mathematical_status; what structural delta would each sibling reading produce?',
    'Compile the sibling files and diff victim sets, epsilon, and computed types: number_reading removes the victim set entirely (operations defined, no exclusion left to bear); placeholder_reading retains the number-domain exclusion but grants notational relief, shrinking measured extraction.',
    'Under number_reading this arrangement is historically superseded and its residual enforcement reads as decay; under placeholder_reading the arrangement softens toward coordination-with-residue. The present file''s classification holds only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_delta, conceptual, 'Committer-frame delta across sibling readings of the zero-status kernel.').

omega_variable(
    ontology_arithmetic_separability,
    'Is the number-domain exclusion load-bearing for the coherence of the being/non-being framework, or separable from it?',
    'Search the late-antique and scholastic record for frameworks that admitted operational zero while retaining core ontological commitments; test whether any coherent adaptation existed that contemporaries could have taken.',
    'If separable, the ontological justification functions as cover and the arrangement slides toward pure extraction; if inseparable, part of the measured extraction is the price of the framework''s own coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontology_arithmetic_separability, conceptual, 'Whether the ontological and arithmetic components of the exclusion are structurally separable.').

omega_variable(
    fraud_rationale_substance,
    'Was the cipher-alteration security rationale (the Florence ordinance of 1299 and similar rules) a genuine coordination function or post-hoc cover for guild protection?',
    'Compare documentary-fraud rates and control procedures between zero-using commercial centers (Islamic-world accounting systems, Italian firms trading abroad) and zero-banning jurisdictions.',
    'If procedural controls substituted adequately, the security function is replicable without exclusion and the extraction component dominates; if not, the ban carried real protective value and the coordination half of the hybrid is heavier than the enforcement record suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fraud_rationale_substance, empirical, 'Substance of the record-security justification versus guild-interest cover.').

omega_variable(
    necessity_presentation_status,
    'The arrangement presents as ontological necessity — a limit on what can coherently be thought — rather than as policy; is the presenting frame accurate?',
    'Modal analysis of the non-being principle together with the historical existence of coherent zero-operating frameworks: a purported necessity with actual coherent violators is construction wearing necessity''s dress.',
    'If construction, the arrangement is eligible for false-summit analysis and its enforcement is policy requiring justification rather than nature requiring acknowledgment; the necessity presentation is precisely the mechanism by which the arrangement''s costs escaped scrutiny for centuries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_presentation_status, conceptual, 'Whether the mountain-shaped self-presentation reflects a genuine limit or a constructed rule.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_mathematical_status__parmenidean_rejection, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t0, zero_mathematical_status__parmenidean_rejection, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(zero_tr_t0, observed).
narrative_ontology:measurement(zero_tr_t5, zero_mathematical_status__parmenidean_rejection, theater_ratio, 5, 0.24).
narrative_ontology:measurement_basis(zero_tr_t5, observed).
narrative_ontology:measurement(zero_tr_t10, zero_mathematical_status__parmenidean_rejection, theater_ratio, 10, 0.27).
narrative_ontology:measurement_basis(zero_tr_t10, observed).
narrative_ontology:measurement(zero_tr_t15, zero_mathematical_status__parmenidean_rejection, theater_ratio, 15, 0.3).
narrative_ontology:measurement_basis(zero_tr_t15, observed).
narrative_ontology:measurement(zero_tr_t20, zero_mathematical_status__parmenidean_rejection, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(zero_tr_t20, observed).
narrative_ontology:measurement(zero_tr_t25, zero_mathematical_status__parmenidean_rejection, theater_ratio, 25, 0.33).
narrative_ontology:measurement_basis(zero_tr_t25, observed).
narrative_ontology:measurement(zero_tr_t30, zero_mathematical_status__parmenidean_rejection, theater_ratio, 30, 0.34).
narrative_ontology:measurement_basis(zero_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(zero_be_t0, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(zero_be_t0, observed).
narrative_ontology:measurement(zero_be_t5, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 5, 0.51).
narrative_ontology:measurement_basis(zero_be_t5, observed).
narrative_ontology:measurement(zero_be_t10, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(zero_be_t10, observed).
narrative_ontology:measurement(zero_be_t15, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(zero_be_t15, observed).
narrative_ontology:measurement(zero_be_t20, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(zero_be_t20, observed).
narrative_ontology:measurement(zero_be_t25, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(zero_be_t25, observed).
narrative_ontology:measurement(zero_be_t30, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 30, 0.63).
narrative_ontology:measurement_basis(zero_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(zero_su_t0, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(zero_su_t0, observed).
narrative_ontology:measurement(zero_su_t5, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(zero_su_t5, observed).
narrative_ontology:measurement(zero_su_t10, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(zero_su_t10, observed).
narrative_ontology:measurement(zero_su_t15, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 15, 0.72).
narrative_ontology:measurement_basis(zero_su_t15, observed).
narrative_ontology:measurement(zero_su_t20, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 20, 0.73).
narrative_ontology:measurement_basis(zero_su_t20, observed).
narrative_ontology:measurement(zero_su_t25, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 25, 0.74).
narrative_ontology:measurement_basis(zero_su_t25, observed).
narrative_ontology:measurement(zero_su_t30, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 30, 0.74).
narrative_ontology:measurement_basis(zero_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_mathematical_status__parmenidean_rejection, identity_coordination).
narrative_ontology:affects_constraint(zero_mathematical_status__parmenidean_rejection, zero_mathematical_status__number_reading).
narrative_ontology:affects_constraint(zero_mathematical_status__parmenidean_rejection, zero_mathematical_status__placeholder_reading).

% DUAL FORMULATION NOTE:
% Constraint family zero_mathematical_status decomposes one colloquial label ('the status of zero') into three epsilon-invariant stories: this file (parmenidean_rejection — the exclusion arrangement, epsilon indexed to the standing exclusion regime), zero_mathematical_status__number_reading (the admission arrangement — operational zero with defined rules, negligible extraction once established), and zero_mathematical_status__placeholder_reading (notation-only tolerance — intermediate victim relief, number-domain exclusion retained). The colloquial label conflates an ontological thesis, an operational thesis, and a notational practice; each gets its own beneficiaries, victims, and type. This reading links to both siblings; the number_reading is the downstream arrangement that ultimately displaced this one, and the placeholder_reading is the compromise position whose narrowness this reading's rigidity created.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
