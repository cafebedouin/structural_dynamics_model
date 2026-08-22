% ============================================================================
% CONSTRAINT STORY: zero_mathematical_status__parmenidean_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: zero_mathematical_status__parmenidean_rejection
 *   human_readable: Parmenidean Rejection of Zero as a Number
 *   domain: history_of_mathematics/philosophy_of_mathematics
 *
 * SUMMARY:
 *   This story authors the Parmenidean-rejection reading of the contested
 *   zero kernel: the position, traceable to the metaphysics of 'nothing
 *   cannot exist' and the classical arithmos definition of number as
 *   plurality-of-units, that zero cannot be admitted as a genuine number
 *   because doing so requires quantifying non-being, a logical incoherence.
 *   This is a live historical-philosophical position with real institutional
 *   force in the traditions that held it — not the number_reading (which
 *   grants zero full arithmetic status per Brahmagupta) and not the
 *   placeholder_reading (which sidesteps the metaphysical question by
 *   treating zero as a mere notational device). Under this reading, the
 *   standing arrangement is the enforced exclusion of zero from the number
 *   domain, defended by scholastic and disciplinary authorities and paid for
 *   by everyone who needed positional arithmetic to function efficiently.
 *
 * KEY AGENTS:
 *   - scholastic_authorities_of_being: institutional agenda-setter defending the coherence of number-as-plurality
 *   - geometric_number_theorists and guild_calculators: incumbent beneficiaries of the status quo toolkit
 *   - merchant_arithmeticians and positional_notation_adopters: primary payers, denied notational efficiency
 *   - algebra_students: powerless payers inheriting pedagogical confusion
 *   - indian_and_islamic_mathematical_traditions: excluded voices holding a coherent alternative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_mathematical_status__parmenidean_rejection, 0.62).
domain_priors:suppression_score(zero_mathematical_status__parmenidean_rejection, 0.71).
domain_priors:theater_ratio(zero_mathematical_status__parmenidean_rejection, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, extractiveness, 0.62).
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_mathematical_status__parmenidean_rejection, tangled_rope).
narrative_ontology:human_readable(zero_mathematical_status__parmenidean_rejection, "Parmenidean Rejection of Zero as a Number").
narrative_ontology:topic_domain(zero_mathematical_status__parmenidean_rejection, "history_of_mathematics/philosophy_of_mathematics").

domain_priors:requires_active_enforcement(zero_mathematical_status__parmenidean_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_mathematical_status__parmenidean_rejection, 'b06543a4-351b-492b-86fb-dc27275c1dd7').
narrative_ontology:cs_kernel_codification('b06543a4-351b-492b-86fb-dc27275c1dd7', distributed).
narrative_ontology:cs_authority_grounding('b06543a4-351b-492b-86fb-dc27275c1dd7', lineage).
narrative_ontology:cs_interpretation_layer_present('b06543a4-351b-492b-86fb-dc27275c1dd7').
narrative_ontology:cs_reading_relation('b06543a4-351b-492b-86fb-dc27275c1dd7', zero_mathematical_status__number_reading, coexists_with).
narrative_ontology:cs_reading_relation('b06543a4-351b-492b-86fb-dc27275c1dd7', zero_mathematical_status__placeholder_reading, influences).
narrative_ontology:cs_axiom('b06543a4-351b-492b-86fb-dc27275c1dd7', foundational, being_cannot_arise_from_non_being).
narrative_ontology:cs_axiom_status(being_cannot_arise_from_non_being, holdable).
narrative_ontology:cs_axiom_grounding('b06543a4-351b-492b-86fb-dc27275c1dd7', being_cannot_arise_from_non_being, deontological).
narrative_ontology:cs_axiom('b06543a4-351b-492b-86fb-dc27275c1dd7', foundational, number_requires_plurality_of_units).
narrative_ontology:cs_axiom_status(number_requires_plurality_of_units, overridden).
narrative_ontology:cs_axiom_grounding('b06543a4-351b-492b-86fb-dc27275c1dd7', number_requires_plurality_of_units, conventional).
narrative_ontology:cs_reference_frame('b06543a4-351b-492b-86fb-dc27275c1dd7', classical_arithmos_plurality_of_units).
narrative_ontology:cs_drift_state('b06543a4-351b-492b-86fb-dc27275c1dd7', post_positional_notation_diffusion, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('b06543a4-351b-492b-86fb-dc27275c1dd7', '').
narrative_ontology:cs_kernel_id(zero_mathematical_status__parmenidean_rejection, zero_mathematical_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_mathematical_status__parmenidean_rejection, scholastic_authorities_of_being).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__parmenidean_rejection, geometric_number_theorists).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__parmenidean_rejection, guild_calculators).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, merchant_arithmeticians).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, positional_notation_adopters).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, algebra_students).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, indian_and_islamic_mathematical_traditions_seeking_recognition).
narrative_ontology:constraint_vindicates(zero_mathematical_status__parmenidean_rejection, being_cannot_arise_from_non_being).
narrative_ontology:constraint_vindicates(zero_mathematical_status__parmenidean_rejection, number_is_a_plurality_of_units).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the classical Greek definition of number as 'a plurality composed of units' (arithmos), inherited through Euclid and Aristotle. They adjudicate what counts as legitimate mathematical entity within the philosophical curriculum, and their authority over what is teachable rests on the coherence of number-as-being. Admitting zero as a number would require conceding that non-being can be counted, which threatens the ontological ground on which their entire disciplinary authority sits.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, scholastic_authorities_of_being, agenda_setter,
    institutional, generational, identity_locked, continental).

% Practice a mathematics grounded in ratios, magnitudes, and geometric construction where zero has no natural referent (there is no line of zero length worth naming). They benefit from a framework that keeps their existing toolkit central and unchallenged by an imported arithmetic that would demote geometric proof to a secondary method.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, geometric_number_theorists, beneficiary,
    organized, biographical, constrained, regional).

% Professional reckoners (abacists, counting-board masters) whose livelihoods depend on manual computation methods that do not require a zero symbol. A rejection of zero as a legitimate number protects their trade's monopoly on complex calculation against a cheaper, faster written positional method that any literate merchant could learn.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, guild_calculators, beneficiary,
    organized, biographical, constrained, local).

% Traders and bookkeepers who need efficient methods for recording debt, empty accounts, and compound transactions across long trade routes. Denied a coherent zero, they must use clumsy circumlocutions (blank spaces, verbal placeholders, separate ledgers for null balances) that introduce transcription errors and slow commerce. Their exit is constrained: they can quietly adopt foreign positional methods but risk censure from scholarly and religious authorities who treat the imported system as philosophically suspect.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, merchant_arithmeticians, payer,
    moderate, biographical, constrained, regional).

% Scribes and early adopters of Hindu-Arabic numerals who need a placeholder digit to write numbers like 105 unambiguously. Operating under a rejection of zero's number-status, they face active resistance from institutions that treat their notation as a foreign, ontologically confused import; some are accused of corrupting sound reasoning by writing 'nothing' as though it were a thing.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, positional_notation_adopters, payer,
    moderate, generational, constrained, continental).

% Learners attempting to master equation-solving are taught to avoid or work around zero rather than operate on it directly, since a+0=a and a*0=0 are not stable teachable rules under a framework that denies zero numberhood. They inherit confusion, memorize special-case workarounds, and cannot progress to the algebraic generality that a settled zero would provide.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, algebra_students, payer,
    powerless, biographical, trapped, regional).

% Mathematicians such as Brahmagupta had already formalized zero's arithmetic centuries earlier within a different metaphysical tradition (Indian number theory did not carry the same being/non-being prohibition). Their work circulates via translation but is treated by the Parmenidean-rejection authorities as a philosophically naive import rather than a competing, coherent number-reading — their voice is present in translated texts but structurally discounted in the adjudicating institutions.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, indian_and_islamic_mathematical_traditions_seeking_recognition, excluded,
    organized, generational, constrained, continental).

% Historians and mathematicians looking back can see that the rejection delayed the systematic adoption of algebra and positional arithmetic in regions where it held sway, while noting that the underlying philosophical worry (can non-being be quantified?) was a genuine conceptual problem, not merely obstruction for its own sake.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, later_algebraists_and_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_mathematical_status__parmenidean_rejection, diffuse).
narrative_ontology:fixing_cost_class(zero_mathematical_status__parmenidean_rejection, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The rejection coordinates a philosophically consistent definition of number across geometry, logic, and pedagogy — anyone trained in the tradition can rely on 'number means plurality of units' holding uniformly across all mathematical contexts, avoiding the incoherence of admitting an entity that is simultaneously a number and the absence of quantity.
% TRANSFER_FUNCTION: Moves computational efficiency and notational flexibility away from merchants, scribes, and algebra students (who bear transcription errors, workaround complexity, and blocked generality) toward incumbent geometric theorists and guild calculators (who retain disciplinary centrality and trade monopoly) and toward scholastic authorities (who retain adjudicative control over what counts as legitimate mathematics).
% ABSENT_VOICES: Indian and Islamic mathematicians who had already worked out zero's arithmetic are present only as translated, secondhand texts; their coherent alternative framework is not admitted into the room where legitimacy is adjudicated, so the debate proceeds as though the only options are the classical position and vague foreign confusion.
% DISAPPEARANCE_RATIONALE: If the rejection vanished, positional notation and Brahmagupta-style arithmetic could be adopted immediately without institutional censure; merchant bookkeeping, algebraic pedagogy, and eventually calculus (which needs a coherent zero and limit-to-zero concept) would proceed on a faster timeline. The rejection is not a fact the world runs on — the world changed considerably once it lapsed.
% FOUNDING_PROBLEM: It was built to preserve a coherent metaphysics of being in which quantity is always quantity-of-something; admitting 'nothing' as a countable number seemed to license contradictions (non-being existing, quantified absence) that threatened the logical foundations classical philosophy had built mathematics upon.
% FOUNDING_PROBLEM_CORROBORATION: Historians of mathematics (outside the scholastic tradition that authored the rejection) document that the ontological worry was substantively resolved once zero was treated as a formal element of an algebraic structure rather than a metaphysical claim about being — a resolution attested by the mathematical traditions the rejection excluded (Indian and Islamic algebraists) centuries before the rejection's own tradition conceded the point. No party still practicing mathematics attests the founding problem as unresolved; the scholastic authorities who benefited from it are the only ones who ever affirmed it as live, and even they abandoned the position within a few centuries.
narrative_ontology:disappearance_verdict(zero_mathematical_status__parmenidean_rejection, world_rearranges).
narrative_ontology:founding_problem_status(zero_mathematical_status__parmenidean_rejection, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_mathematical_status__parmenidean_rejection, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zero_mathematical_status__parmenidean_rejection, 'none', 1).
narrative_ontology:epsilon_provenance(zero_mathematical_status__parmenidean_rejection, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.62) reflects the real cost imposed on commerce, notation, and pedagogy by excluding a functional zero, while suppression (0.71) reflects the active philosophical and institutional policing required to keep positional-notation imports from simply displacing the rejection on efficiency grounds alone — this is not a passive oversight but an actively defended position. Accessibility collapse is moderate-low (0.35) because alternatives (imported positional systems, geometric workarounds) remained visibly available throughout the interval; the rejection never fully closed off the alternative, which is part of why it eventually gave way. Resistance is high (0.78) because merchants, scribes, and mathematicians in contact with Indian/Islamic sources persistently pushed back against the prohibition. Theater ratio rises through the middle of the interval (peaking near 0.45) as scholastic defenses of the position become increasingly performative — reasserting the classical definition rhetorically even as practical computation quietly migrated toward positional methods at the margins — then eases as the position loses ground.
 *
 * PERSPECTIVAL GAP:
 *   From the scholastic authority's seat, the rejection is required by rigorous ontology — admitting zero would be intellectually dishonest. From the merchant's seat, the same rule is an arbitrary obstacle to keeping honest books. The engine should compute these as different seat-level classifications from the same structural data, not reconcile them to a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Scholastic authorities and incumbent practitioners (geometric theorists, guild calculators) sit near the beneficiary end: the rejection protects their disciplinary and economic position without them bearing its costs. Merchants, notation adopters, and students sit near the target end: they bear transcription costs, pedagogical incoherence, and institutional censure for working around the prohibition. The excluded traditions are structurally different again — they are not paying a cost internal to this reading's institutions so much as being denied entry to the adjudicating conversation entirely, which is why they are marked excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a genuine metaphysical worry about quantifying non-being) was live and serious when first raised — this is not manufactured extraction dressed as philosophy. But by the time the rejection had hardened into institutional practice defended primarily by guild and disciplinary incumbents, the coordination function (philosophical coherence) had become secondary to the extraction function (protecting guild calculators' trade monopoly and geometric theorists' disciplinary centrality). Treating this as tangled_rope rather than pure snare respects that a genuine coordination problem existed at the root; treating it as tangled_rope rather than pure rope respects that its persistence past the point of resolution required active enforcement against payers who had a demonstrably workable alternative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_metaphysics_vs_guild_protection,
    'Was the Parmenidean rejection primarily a genuine, good-faith metaphysical position, or did it persist mainly because it protected the economic and disciplinary interests of guild calculators and geometric theorists after the philosophical question had been adequately answered elsewhere?',
    'Comparative historical analysis of when and where the rejection weakened fastest — if it weakened first in regions with least guild-calculator economic power and most exposure to Indian/Islamic mathematics, that supports the guild-protection reading over the pure-metaphysics reading.',
    'If protection-driven, the tangled_rope classification is strongly supported (real coordination origin, later captured for extraction). If purely philosophical and resolved only by genuine argument, the case for calling it extraction at all weakens toward a contested rope-that-was-wrong-but-not-extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_metaphysics_vs_guild_protection, conceptual, 'Whether guild/disciplinary capture or genuine philosophy explains the rejection''s persistence.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does the parmenidean_rejection reading merely coexist with the number_reading as a historically superseded but internally coherent position, or does its core premise (nothing cannot be quantified) logically foreclose the number_reading''s core premise (zero has defined arithmetic operations) within any single coherent framework?',
    'Formal analysis of whether ''a x 0 = 0'' and ''quantity of non-being is incoherent'' can be jointly held without contradiction in a single logical system, versus whether they are simply two different systems that never needed to interoperate.',
    'If truly foreclosing, no party can coherently hold both readings at once, which changes how the kernel''s contest should be modeled (mutually exclusive commitments rather than parallel traditions). If merely coexisting historically (different traditions, no shared framework ever attempted), the coexists_with relation is the more accurate structural fact, which is the position this story takes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether the rejection reading logically forecloses or merely historically preceded the number reading.').

omega_variable(
    placeholder_reading_as_stealth_concession,
    'Is the placeholder_reading (zero as notational device, not a number) a genuinely distinct third position, or is it a face-saving retreat that lets the rejection''s defenders concede zero''s practical utility without conceding the ontological point — effectively a transitional cover story on the path to the number_reading?',
    'Trace historical adoption sequence: did practitioners who used positional notation but denied zero''s numberhood later convert to the full number_reading, and did they cite the placeholder position as a deliberate bridge?',
    'If the placeholder reading functioned as a stealth concession, then this rejection reading''s institutional persistence was already hollowed out well before its formal abandonment — raising the theater_ratio interpretation for the later portion of the interval and supporting the founding_problem_status of ''dead'' well before the nominal end date.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(placeholder_reading_as_stealth_concession, conceptual, 'Whether the placeholder reading is a genuine third position or a transitional concession from the rejection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_mathematical_status__parmenidean_rejection, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t0, zero_mathematical_status__parmenidean_rejection, theater_ratio, 0, 0.2).
narrative_ontology:measurement(zero_tr_t20, zero_mathematical_status__parmenidean_rejection, theater_ratio, 20, 0.28).
narrative_ontology:measurement(zero_tr_t40, zero_mathematical_status__parmenidean_rejection, theater_ratio, 40, 0.36).
narrative_ontology:measurement(zero_tr_t60, zero_mathematical_status__parmenidean_rejection, theater_ratio, 60, 0.42).
narrative_ontology:measurement(zero_tr_t80, zero_mathematical_status__parmenidean_rejection, theater_ratio, 80, 0.45).
narrative_ontology:measurement(zero_tr_t100, zero_mathematical_status__parmenidean_rejection, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(zero_be_t0, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(zero_be_t20, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(zero_be_t40, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(zero_be_t60, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(zero_be_t80, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 80, 0.63).
narrative_ontology:measurement(zero_be_t100, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(zero_su_t0, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(zero_su_t20, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(zero_su_t40, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(zero_su_t60, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 60, 0.78).
narrative_ontology:measurement(zero_su_t80, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 80, 0.72).
narrative_ontology:measurement(zero_su_t100, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 100, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_mathematical_status__parmenidean_rejection, identity_coordination).
narrative_ontology:affects_constraint(zero_mathematical_status__parmenidean_rejection, zero_mathematical_status__number_reading).
narrative_ontology:affects_constraint(zero_mathematical_status__parmenidean_rejection, zero_mathematical_status__placeholder_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings decomposed from the natural-language label 'the status of zero as a number,' per the ε-invariance principle. number_reading (Brahmagupta arithmetic, low-to-moderate ε, closer to rope/mountain depending on scope) and placeholder_reading (zero as notation only, distinct victim/beneficiary structure) are separate stories with their own ε values and classifications. All three are linked here and should reciprocally link back to this constraint_id in their own network.affects_constraints arrays, per the BGS gold-standard pattern for kernel decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
