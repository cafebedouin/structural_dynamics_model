% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_archiving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_archiving, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: temple_sacrifice_obligation__study_as_archiving
 *   human_readable: Post-Temple Sacrificial Obligation — Study-as-Archiving Reading
 *   domain: religious/halakhic/commitment_systems
 *
 * SUMMARY:
 *   After the destruction of the Second Temple in 70 CE, the tradition faced
 *   a commandment it holds binding with no venue in which to perform it. This
 *   story instantiates one reading of that situation — the archiving reading:
 *   the obligation remains fully in force, every year of non-performance is
 *   genuine non-compliance, and the study of sacrificial law is assigned a
 *   preservative function — keeping the procedure executable for a future
 *   restoration — while conferring no discharge of the debt. The arrangement
 *   under contest is therefore a standing regime: an authority structure that
 *   maintains the bindingness of an unperformable law, a community that
 *   services the unmet command liturgically and pedagogically, and an archive
 *   that may or may not ever be spent. The epsilon authored here is indexed
 *   to this reading's own lights over that standing arrangement — not to the
 *   sibling readings' alternatives, which are separate constraints linked
 *   through the network. Claim and metrics are authored independently: the
 *   claimed type states what this story takes the structure to be; the
 *   metrics state what it descriptively does. KEY AGENTS (by structural
 *   relationship): - rabbinic_halakhic_authority: Agenda-setting authority
 *   (institutional/identity_locked) — maintains the ruling, transmits the
 *   curriculum, lives under the same unmet command -
 *   post_temple_observant_community: Primary bearer
 *   (organized/identity_locked) — services the unmet command liturgically and
 *   pedagogically - unfulfilled_divine_command: The outstanding obligation
 *   itself (non-agent) — receives no performance in any year of the interval
 *   - future_restoration_generation: Intended heir of the archive (non-agent)
 *   — would execute the preserved procedure -
 *   non_rabbinic_israelite_lineages: Excluded dissent (moderate/mobile) —
 *   answered the same question by other paths - analytical_observer:
 *   Analytical seat (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, 0.55).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_archiving, 0.45).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_archiving, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, extractiveness, 0.55).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_archiving, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_archiving, "Post-Temple Sacrificial Obligation — Study-as-Archiving Reading").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_archiving, "religious/halakhic/commitment_systems").

domain_priors:requires_active_enforcement(temple_sacrifice_obligation__study_as_archiving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_archiving, 'b8b7a3fa-03fa-490c-a989-c321844d5d5a').
narrative_ontology:cs_kernel_codification('b8b7a3fa-03fa-490c-a989-c321844d5d5a', fixed_text).
narrative_ontology:cs_authority_grounding('b8b7a3fa-03fa-490c-a989-c321844d5d5a', lineage).
narrative_ontology:cs_interpretation_layer_present('b8b7a3fa-03fa-490c-a989-c321844d5d5a').
narrative_ontology:cs_reading_relation('b8b7a3fa-03fa-490c-a989-c321844d5d5a', temple_sacrifice_obligation__study_as_occupation, forecloses).
narrative_ontology:cs_reading_relation('b8b7a3fa-03fa-490c-a989-c321844d5d5a', temple_sacrifice_obligation__messianic_suspension, forecloses).
narrative_ontology:cs_axiom('b8b7a3fa-03fa-490c-a989-c321844d5d5a', foundational, obligation_binding_unperformed_post_temple).
narrative_ontology:cs_axiom_status(obligation_binding_unperformed_post_temple, holdable).
narrative_ontology:cs_axiom_grounding('b8b7a3fa-03fa-490c-a989-c321844d5d5a', obligation_binding_unperformed_post_temple, deontological).
narrative_ontology:cs_axiom('b8b7a3fa-03fa-490c-a989-c321844d5d5a', foundational, study_preserves_without_discharge).
narrative_ontology:cs_axiom_status(study_preserves_without_discharge, holdable).
narrative_ontology:cs_axiom_grounding('b8b7a3fa-03fa-490c-a989-c321844d5d5a', study_preserves_without_discharge, deontological).
narrative_ontology:cs_reference_frame('b8b7a3fa-03fa-490c-a989-c321844d5d5a', sinaitic_obligation_unabridged).
narrative_ontology:cs_drift_state('b8b7a3fa-03fa-490c-a989-c321844d5d5a', contemporary, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('b8b7a3fa-03fa-490c-a989-c321844d5d5a', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, rabbinic_halakhic_authority).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, future_restoration_generation).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, post_temple_observant_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, rabbinic_halakhic_authority).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_archiving, permanence_of_sinaitic_obligation).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_archiving, oral_torah_transmission_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and transmits the ruling that the offering commandments remain in force despite the altar's absence, and organizes the study of sacrificial law as preservation of procedure for a future restoration. Its sages live under the same unmet command as the communities they address. Its office rests on a claim of unbroken transmission from Sinai; reopening or closing the question by fiat would strain that claim, so the ruling is maintained and re-taught rather than revised. What accrues to it is jurisdiction: the permanently open question is the docket it administers.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, rabbinic_halakhic_authority, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__study_as_archiving, rabbinic_halakhic_authority, payer).

% Carries the command unmet: it recites the order of the offerings in the daily liturgy, mourns the altar's absence on the calendar of fasts, funds and staffs the study tractates that keep the procedure legible, and passes the whole inheritance to its children unfulfilled. Individual departure has historically meant leaving the covenantal community altogether, so the weight is carried communally rather than set down.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, post_temple_observant_community, payer,
    organized, civilizational, identity_locked, global).

% The command itself: what it requires is not rendered in any year of the interval. It cannot compel its own performance and cannot be withdrawn; it stands as an open item on the covenant's ledger, acknowledged aloud in liturgy but discharged by nothing done on earth.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command).

% The generation in which the altar stands again would receive a working inheritance — texts, measurements, sequences, calendar logic — kept at cost by every predecessor. It does not yet exist; the tradition asserts its claim on the present on its behalf, and no one speaks for it except through that assertion.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, future_restoration_generation, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__study_as_archiving, future_restoration_generation).

% Communities that answered the same question by other paths: the Samaritans, who kept an altar of their own on Gerizim and never accepted the rabbinic settlement; the Karaites, who rejected the oral transmission through which the ruling travels. Their persistence shows the question was answerable otherwise; they stand outside the conversation in which the ruling was fixed and would contest its terms if admitted.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, non_rabbinic_israelite_lineages, excluded,
    moderate, civilizational, mobile, regional).

% Historians and scholars of religion who trace how the ruling consolidated, what carrying it has cost the communities that hold it, and what each rival reading would change. They observe the arrangement without standing under the command.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, analytical_observer, observer,
    analytical, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__study_as_archiving, rabbinic_halakhic_authority).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__study_as_archiving, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves an executable body of ritual knowledge — texts, measurements, sequences, calendar logic — across an open-ended interval in which it cannot be used, and keeps a dispersed community oriented to a single restoration it awaits together. The daily liturgical rehearsal and the study curriculum solve, once and centrally, the problem of transmitting unusable-but-needed procedure across generations.
% TRANSFER_FUNCTION: Moves study-labor, liturgical attention, and institutional resources from each generation of the observant community into the preserved corpus and the offices that teach it. Nothing in the arrangement discharges the underlying debt: the offerings themselves are not rendered, so the obligation transfers forward intact to each new generation. Adjudicative authority over the open question concentrates in the transmitting institutions.
% ABSENT_VOICES: Holders of the rival readings — those who would say study already occupies the obligation, or that it lies suspended — are present in the tradition's own margins but absent from the settled ruling; the Samaritan and Karaite lineages, who never accepted the adjudicating framework at all, are wholly outside it. The command's addressee-side is voiced only through the institutions that claim to transmit it.
% DISAPPEARANCE_RATIONALE: If the ruling and its archival apparatus vanished overnight, the daily liturgy would lose the offering-order recitations, the fast days would lose their center of gravity, the study curriculum would shed its sacrificial tractates, and the transmitting institutions would lose the permanently open question that anchors their docket. Some successor settlement — occupation, suspension, or quiet abandonment — would take the space within a generation or two, and the preserved procedure would decay beyond recovery within a few more.
% FOUNDING_PROBLEM: In 70 CE the Romans destroyed the Second Temple, leaving a commandment the tradition holds obligatory with no venue in which to perform it: how does a covenant community live under a law it cannot keep, and how does it keep the possibility of future keeping alive?
% FOUNDING_PROBLEM_CORROBORATION: The founding event is attested far outside the benefiting parties: Roman historical accounts (Josephus; the Arch of Titus) and the archaeological record of the destruction layer. The problem's persistence — no functioning altar on the mount — is publicly checkable to this day. The non-rabbinic lineages and independent academic historians attest that the problem remains unsolved while disputing the rabbinic settlement of it; no party inside the arrangement is the sole witness to either the event or its duration.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_archiving, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_archiving, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_archiving, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_archiving, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_archiving, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_archiving_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_obligation__study_as_archiving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55 — moderate, per the reading's own structure: the community carries a compliance deficit that can never be cleared within history, and servicing it (liturgy, curriculum) is substantial labor, but the arrangement also delivers a real preservation good and commands broad voluntary fidelity. Suppression is 0.45: rival settlements exist and are practiced outside the framework, but inside it the ruling is settled and deviation carries communal cost. Suppression is authored as a raw structural property and is deliberately not scaled by power or scope — the engine owns any scaling, and it scales extractiveness alone. Theater ratio 0.35: the daily recitation of the offerings is functional memory-keeping with a performative share that grows as practical anticipation recedes. Accessibility collapse 0.45: alternatives demonstrably persisted across the whole interval — a rival altar, a rival hermeneutic, modern denominational divergence — so understanding the arrangement does not close off exits. Resistance 0.5: schism and repudiation recur across nineteen centuries. The temporal series share one grid (ten points, three metrics, every metric authored at every point): extractiveness climbs through the codification era as the open file consolidates, then plateaus with a slight modern softening as partial alternative venues of covenantal expression emerged; theater creeps monotonically upward; suppression peaks at Talmud-closure and decays as the ruling became self-evident inside the framework. The suppression series is included because this story specifically traces enforcement-capacity change — heavy adjudication effort during consolidation, then routinization — not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the transmitting institutions' seat the arrangement is fidelity itself: keeping faith with a command that cannot be kept is obedience in its purest form. From the community's seat it is an inherited weight: a debt serviced in every generation and dischargeable by no one alive. From the command's seat — a non-agent — it is a bare outstanding balance. The engine computes these divergences from the declared power, exit, and role data; the divergence between the identity-locked payer and the identity-locked administrator, holding the same tradition from opposite sides of the ledger, is the perspectival structure this story exists to register.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutions are declared beneficiaries with identity-locked exit: what the arrangement preserves is their own warrant, so their derived directionality sits near the beneficiary pole and effective extraction damps toward subsidy — they are paid in jurisdiction. The community is declared bearing-party with identity-locked exit: trapped or identity-locked targets sit nearer the full-target pole than mobile ones, so the deficit lands on them at nearly full weight. The command and the future generation are authored as non-agents and feed no directionality — a command cannot collect and an unbuilt generation cannot receive — but their declaration fixes the story's moral geometry. The dissent lineages are an excluded seat outside the arrangement: they document that exits existed without entering the computation. No directionality overrides are needed; the derivation from declared structure produces the correct relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   Two mislabels are blocked. Read from inside the tradition, the arrangement presents as pure coordination — fidelity, memory, transmission — which would land it as rope and miss the permanently open ledger the institutions administer. Read from outside, it presents as pure jurisdiction-preservation — a debt kept open because the open file is the office — which would land it as snare and miss the genuine preservation good the archive delivers. Tangled_rope holds both halves: the same structure that keeps the procedure executable keeps the docket open. On obsolescence: the founding problem is live (no altar exists), so no dead-mandate flag fires. But the arrangement's termination condition is theological, not procedural — there is no declared sunset clause, only an event no institution controls. That cuts both ways: if restoration came, the arrangement would dissolve cleanly and quickly, which counts against reading its persistence as mere self-interest; yet the same theology guarantees the institution never faces that test, which is why the undeclared sunset belongs in the omega record rather than in a has_sunset_clause flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_contest,
    'Which reading of the kernel — archiving, occupation, or suspension — correctly characterizes the obligation''s status during the Temple''s absence?',
    'Internal halakhic adjudication has not closed the question in nineteen centuries; in-principle resolution arrives only with restoration itself, when the archive either proves necessary or is revealed to have been substitutable. Until then the contest is settled only within particular communities'' commitments, not across them.',
    'If the occupation reading prevails, the standing deficit this story measures disappears — study discharges the debt and the arrangement loses its costly side. If suspension prevails, the administered obligation dissolves entirely and the constraint lapses. The epsilon authored here is indexed to the archiving reading alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_contest, conceptual, 'Kernel contest among three mutually exclusive readings of the post-Temple obligation; this story instantiates one of them.').

omega_variable(
    bindingness_of_unperformable_command,
    'Can a command bind its addressees during a period in which compliance is impossible — does the obligation generate continuous non-compliance, or does impossibility itself modify the command''s force?',
    'Textual and conceptual analysis of the tradition''s own materials — the principle that the command''s force does not lapse with circumstance, weighed against the liturgy''s own counting of years of non-performance; no external data can settle it.',
    'If impossibility suspends force, the archiving reading collapses toward the suspension sibling and the measured burden falls toward zero; if bindingness survives, the deficit is real and permanent, and the arrangement''s costly side is structural rather than contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bindingness_of_unperformable_command, conceptual, 'Whether bindingness survives unperformability — the load-bearing premise distinguishing this reading from the suspension sibling.').

omega_variable(
    restoration_premise_contingency,
    'Will the restoration the archive serves ever occur, and does the archival function retain its justification if the interval extends indefinitely?',
    'None available short of the event itself; the premise is unfalsifiable within history, which is precisely what insulates the arrangement''s transitional justification from refutation.',
    'If restoration is treated as never-arriving, the transitional justification erodes and the maintenance component dominates — pushing the arrangement toward inertial persistence; if treated as imminent, the transitional justification strengthens and the preservation function carries the classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_premise_contingency, empirical, 'Contingency of the archival function''s value on an unresolvable theological premise.').

omega_variable(
    archive_fidelity_under_disuse,
    'Is the preserved corpus sufficient to restore the system — the Talmud itself records lost details such as the incense composition and certain species identifications — and does fidelity decay with each generation of purely textual transmission?',
    'Comparative analysis of the corpus against surviving practice fragments (the Samaritan Passover offering offers a partial external check), plus philological reconstruction of the disputed details.',
    'If the archive is materially insufficient, the preservation function fails on its own terms and the arrangement''s coordination side weakens — the reading''s justification narrows to memory-keeping alone, and the costly side dominates the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archive_fidelity_under_disuse, empirical, 'Whether the archive actually preserves what restoration would require.').

omega_variable(
    kernel_vs_authority_framing,
    'Is the contested kernel the sacrificial command itself, or the authority structure''s claim to adjudicate it — and does the choice of framing change the classification?',
    'Run both framings: under the command-framing the parties are the community and the command; under the authority-framing the parties are the community and the institution claiming interpretive monopoly. The signal guiding the initial choice: the kernel context names the obligation, not the academy, as the contested object.',
    'Under the authority-framing the arrangement reads as jurisdiction-preservation and the burden on the community weighs heavier; under the command-framing the preservation function dominates and the burden moderates. The authored values assume the command-framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_vs_authority_framing, conceptual, 'CS-framing under-determination: which stabilized commitment is the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_archiving, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t70, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 70, 0.12).
narrative_ontology:measurement(temp_tr_t250, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 250, 0.16).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 500, 0.2).
narrative_ontology:measurement(temp_tr_t750, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 750, 0.23).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1000, 0.26).
narrative_ontology:measurement(temp_tr_t1250, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1250, 0.28).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1500, 0.3).
narrative_ontology:measurement(temp_tr_t1750, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1750, 0.32).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 2000, 0.34).
narrative_ontology:measurement(temp_tr_t2026, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 2026, 0.35).

% Extraction over time
narrative_ontology:measurement(temp_be_t70, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 70, 0.42).
narrative_ontology:measurement(temp_be_t250, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 250, 0.46).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 500, 0.5).
narrative_ontology:measurement(temp_be_t750, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 750, 0.52).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1000, 0.54).
narrative_ontology:measurement(temp_be_t1250, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1250, 0.55).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1500, 0.56).
narrative_ontology:measurement(temp_be_t1750, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1750, 0.57).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 2000, 0.56).
narrative_ontology:measurement(temp_be_t2026, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 2026, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t70, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 70, 0.35).
narrative_ontology:measurement(temp_su_t250, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 250, 0.43).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 500, 0.48).
narrative_ontology:measurement(temp_su_t750, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 750, 0.5).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1000, 0.5).
narrative_ontology:measurement(temp_su_t1250, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1250, 0.49).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1500, 0.48).
narrative_ontology:measurement(temp_su_t1750, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1750, 0.47).
narrative_ontology:measurement(temp_su_t2000, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 2000, 0.46).
narrative_ontology:measurement(temp_su_t2026, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 2026, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_archiving, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__messianic_suspension).

% DUAL FORMULATION NOTE:
% The colloquial label 'the sacrificial obligation after the destruction' decomposes into three structurally distinct constraints — the archiving reading (this file), the occupation reading, and the suspension reading — each with its own epsilon, victim set, and classification, per the epsilon-invariance principle. The archiving reading sits mid-family: it shares the occupation reading's refusal to suspend the command but denies study any discharging effect, and shares the suspension reading's orientation toward future restoration while denying that the interim is obligation-free. Family links run through network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
