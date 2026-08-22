% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__abrogating_universal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__abrogating_universal, []).

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
 *   constraint_id: quran_9_5_scope__abrogating_universal
 *   human_readable: Abrogating-Universal Reading of Quran 9:5 — Standing Offensive Jihad Obligation
 *   domain: religious jurisprudence / political theology
 *
 * SUMMARY:
 *   This story models a single legal arrangement as one specific reading
 *   holds it: that verse 9:5 abrogates (nasikh) the earlier Quranic verses
 *   enjoining peaceful coexistence and restraint, establishing offensive war
 *   against polytheist populations — until conversion or formal submission —
 *   as a permanently binding obligation on the polity. Where the arrangement
 *   governs, a jurist establishment administers the hierarchy of abrogating
 *   and abrogated verses and staffs its courts; political-military leadership
 *   draws war warrant, treasury share, and land-allocation authority from it;
 *   fighters and frontier garrison settlers receive its distributions;
 *   unsubmitted non-Muslim populations face a standing ultimatum with no exit
 *   that preserves their religion and autonomy; submitted populations pay
 *   differential taxes under protection contracts; and internal readers who
 *   bind the verse to its seventh-century occasion bear heresy accusations
 *   and worse. Per the epsilon-invariance decomposition rule, this file
 *   authors epsilon for the abrogating-universal arrangement alone; the same
 *   kernel's other readings are separate stories with their own epsilon,
 *   beneficiary/victim sets, and classifications, linked through the network
 *   block. KEY AGENTS (by structural relationship): -
 *   abrogationist_jurist_establishment: Agenda-setting administrator
 *   (institutional/identity_locked) — fixes the operative verse hierarchy and
 *   staffs its application - caliphal_expansionist_leadership: Commander and
 *   chief recipient (powerful/constrained) — draws war warrant and treasury
 *   share from the arrangement - mujahidin_fighters: Fighting beneficiary
 *   (organized/constrained) — receives spoils shares and communal standing -
 *   frontier_garrison_settlers: Settlement beneficiary
 *   (organized/constrained) — holds redistributed land and revenue -
 *   unsubmitted_polytheist_communities: Primary target (powerless/trapped) —
 *   faces the standing ultimatum - treaty_bound_non_muslim_allies:
 *   Conditional target (moderate/constrained) — holds treaties subordinate to
 *   the war schedule - dhimmi_taxpayers: Submitted taxpayer
 *   (moderate/trapped) — pays differential taxes under protection -
 *   internal_dissenting_readers: Suppressed insider
 *   (moderate/identity_locked) — reads the verse as occasion-bound -
 *   targeted_community_representatives: Excluded voice (powerless/trapped) —
 *   objectors with no seat in the determination of their own status -
 *   comparative_jurisprudence_scholars: Analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, 0.74).
domain_priors:suppression_score(quran_9_5_scope__abrogating_universal, 0.78).
domain_priors:theater_ratio(quran_9_5_scope__abrogating_universal, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, extractiveness, 0.74).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, accessibility_collapse, 0.84).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__abrogating_universal, tangled_rope).
narrative_ontology:human_readable(quran_9_5_scope__abrogating_universal, "Abrogating-Universal Reading of Quran 9:5 — Standing Offensive Jihad Obligation").
narrative_ontology:topic_domain(quran_9_5_scope__abrogating_universal, "religious jurisprudence / political theology").

domain_priors:requires_active_enforcement(quran_9_5_scope__abrogating_universal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__abrogating_universal, '18820f6c-4403-4efc-a95b-a9ba82adf4ea').
narrative_ontology:cs_kernel_codification('18820f6c-4403-4efc-a95b-a9ba82adf4ea', fixed_text).
narrative_ontology:cs_authority_grounding('18820f6c-4403-4efc-a95b-a9ba82adf4ea', lineage).
narrative_ontology:cs_interpretation_layer_present('18820f6c-4403-4efc-a95b-a9ba82adf4ea').
narrative_ontology:cs_reading_relation('18820f6c-4403-4efc-a95b-a9ba82adf4ea', quran_9_5_scope__contextual_defensive, forecloses).
narrative_ontology:cs_reading_relation('18820f6c-4403-4efc-a95b-a9ba82adf4ea', quran_9_5_scope__progressive_synthesis, forecloses).
narrative_ontology:cs_axiom('18820f6c-4403-4efc-a95b-a9ba82adf4ea', foundational, nine_five_abrogates_peaceful_verses).
narrative_ontology:cs_axiom_status(nine_five_abrogates_peaceful_verses, holdable).
narrative_ontology:cs_axiom_grounding('18820f6c-4403-4efc-a95b-a9ba82adf4ea', nine_five_abrogates_peaceful_verses, empirically_contingent).
narrative_ontology:cs_axiom('18820f6c-4403-4efc-a95b-a9ba82adf4ea', foundational, offensive_jihad_standing_obligation).
narrative_ontology:cs_axiom_status(offensive_jihad_standing_obligation, holdable).
narrative_ontology:cs_axiom_grounding('18820f6c-4403-4efc-a95b-a9ba82adf4ea', offensive_jihad_standing_obligation, theological).
narrative_ontology:cs_reference_frame('18820f6c-4403-4efc-a95b-a9ba82adf4ea', standing_offensive_jihadic_mandate).
narrative_ontology:cs_drift_state('18820f6c-4403-4efc-a95b-a9ba82adf4ea', contemporary_post_colonial_state_system, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('18820f6c-4403-4efc-a95b-a9ba82adf4ea', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__abrogating_universal, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, caliphal_expansionist_leadership).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, mujahidin_fighters).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, frontier_garrison_settlers).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, abrogationist_jurist_establishment).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, unsubmitted_polytheist_communities).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, treaty_bound_non_muslim_allies).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, dhimmi_taxpayers).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, internal_dissenting_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, dhimmi_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Staffs the courts and teaching circles in which the legal hierarchy of verses is fixed: which revelations remain operative, which are superseded, and what duties follow. Issues rulings on war, treaties, and the status of non-Muslim populations, and trains the judges who apply them. Their scholarly rank, appointments, and students depend on the transmitted framework they administer; stepping outside it costs them the standing that constitutes their vocation.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, abrogationist_jurist_establishment, agenda_setter,
    institutional, generational, identity_locked, global).

% Holds supreme command and treasury authority: declares campaigns, accepts or rejects submissions, collects the reserved fifth of spoils, and grants conquered land. The doctrine supplies the legal warrant and mobilizing legitimacy for these acts; renouncing it would strip their wars of lawful status and their office of a principal claim to obedience.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, caliphal_expansionist_leadership, agenda_setter,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__abrogating_universal, caliphal_expansionist_leadership, beneficiary).

% Serve in campaigns authorized under the doctrine. They receive shares of movable spoils, captives, and occasional land allotments, plus standing in the community as fulfillers of a collective duty. Leaving the ranks mid-campaign carries legal penalty, and declining the duty when called exposes them to censure.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, mujahidin_fighters, beneficiary,
    organized, biographical, constrained, regional).

% Receive allotments of land and revenue from territories brought under the polity's control, often as garrison communities settled at the edges of expansion. Their livelihoods and their descendants' holdings trace to the redistribution the doctrine licenses.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, frontier_garrison_settlers, beneficiary,
    organized, generational, constrained, regional).

% Face a standing ultimatum: accept the polity's religion, accept subordinate status under its protection, or be fought. Their worship, property, and lives remain legally exposed so long as they stay outside submission; flight, fortification, and alliance are the options left to them, each costly and temporary.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, unsubmitted_polytheist_communities, payer,
    powerless, biographical, trapped, continental).

% Live under treaties whose duration and renewal depend on the ruler's judgment once treaty obligations are classified as subordinate to the war duty. Their security rests on executive restraint rather than on a right the doctrine itself guarantees; they pay tribute where demanded and cannot compel renewal.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, treaty_bound_non_muslim_allies, payer,
    moderate, biographical, constrained, regional).

% Have submitted and live under protection contracts: they pay a special head tax and land tax, observe restrictions on worship display and bearing arms, and in exchange are shielded from the warfare the doctrine otherwise licenses against them. Their communal institutions survive inside limits set by the contract; converting removes the tax but dissolves the community's legal personality.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, dhimmi_taxpayers, payer,
    moderate, generational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__abrogating_universal, dhimmi_taxpayers, beneficiary).

% Scholars and teachers who read the same corpus as bounded by its occasion or superseded by its ethical arc. They publish, teach, and advise within the tradition while rejecting the universal-war reading; they face accusations of heresy or betrayal, loss of posts, and in harsher settings prosecution. Their learning, language, and belonging are all inside the tradition they dispute, so leaving it means losing the audience and identity their work addresses.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, internal_dissenting_readers, payer,
    moderate, generational, identity_locked, global).

% Elders, envoys, and spokesmen of the populations the doctrine classifies. They appear in the record as recipients of invitation letters, treaty parties, or defeated delegations; they hold no seat in the juristic sessions where their status is determined, and their objections enter only as facts to be adjudicated.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, targeted_community_representatives, excluded,
    powerless, biographical, trapped, continental).

% Historians and legal comparativists who study how the arrangement functioned across periods and polities — when it operated, when it lay dormant, what it moved and to whom. They take no side in the tradition's internal dispute and hold no stake in its operation.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, comparative_jurisprudence_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__abrogating_universal, caliphal_expansionist_leadership).
narrative_ontology:fixing_cost_class(quran_9_5_scope__abrogating_universal, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts episodic, discretionary raiding into a single standing legal program: it fixes who may be fought and when, who commands, how spoils and captives are divided, how treaties are classified, and what duties fall on those not fighting — solving mobilization, command, and distribution problems for the polity without case-by-case negotiation.
% TRANSFER_FUNCTION: Moves land, movable wealth, captives, and labor from populations outside submission to the fighting polity — treasury shares, fighter shares, settler allotments — and moves a recurring head-and-land tax from submitted non-Muslim populations to the treasury; it also moves the risk of death onto both the polity's fighters and the targeted populations.
% ABSENT_VOICES: The populations the doctrine classifies have no seat in the juristic process that fixes their status; their consent enters only as submission, treaty, or defeat. Inside the tradition, readers who bound the verse to its occasion speak under the shadow of heresy charges, so their dissent is voiced at personal cost rather than counted as a vote.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, the polity's wars would lose their standing legal warrant and revert to ad hoc decisions; the spoils-and-allotment economy built on redistributed land would lose its title regime; juristic authority would have to reorganize around whichever reading of the verse survived; and the ultimatum structure facing unsubmitted populations would lift, replaced by ordinary diplomacy.
% FOUNDING_PROBLEM: After the conquest of Mecca, the Medinan polity faced a window in which Arab tribes that had broken treaties or harbored its enemies could regroup; verse 9:5 answered that window — a proclaimed grace period of four sacred months after which treaty-breakers would be fought — as part of consolidating the peninsula under a single authority.
% FOUNDING_PROBLEM_CORROBORATION: Early maghazi and sirah compilations, preserved treaty documents, and modern historiography of the Riddah wars and peninsular consolidation all locate the occasion in the specific seventh-century coalition crisis. No source outside the doctrine's beneficiary set attests a presently-live equivalent of that crisis; the claim that the underlying problem is perennial is advanced only by the reading's own adherents, and that self-attestation is itself the signal.
narrative_ontology:disappearance_verdict(quran_9_5_scope__abrogating_universal, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__abrogating_universal, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__abrogating_universal, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_9_5_scope__abrogating_universal, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__abrogating_universal, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__abrogating_universal_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_9_5_scope__abrogating_universal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The scalars characterize the arrangement's operative profile: extraction 0.74 (life, property, labor, and religious liberty transfer from unsubmitted populations; differential taxation from submitted ones), suppression 0.78 (persistence requires apostasy and heresy penalties, takfir against dissenting readers, and war against refusal — permanent coexistence is precisely what the doctrine exists to exclude), theater 0.35 (real enforcement where operative; juristic and rhetorical maintenance where capacity lapsed). Accessibility collapse is high (0.84) because the divine-command framing closes internal alternatives near-completely — rejection is heresy, not policy disagreement — and reduces the target's choice set to conversion, submission, or war; resistance is moderate (0.58) because target military resistance repeatedly stalled expansion and internal contestation never died. The temporal series runs on one shared grid (years since revelation, eight points, all three metrics authored at every point) and tracks operativity-weighted levels: extraction peaks in the expansion centuries, troughs in the colonial-era dormancy — where theater peaks, the doctrine being maintained performatively without capacity — and partially recovers with modern revivalist operativity. The suppression_requirement series is authored because enforcement capacity genuinely changed across the interval (built up during expansion, decayed under dormancy, partially rebuilt recently); it is distinct from the scalar suppression, which measures the structural coercive force embedded in the arrangement wherever it governs. Claim and metrics are independent: I claim tangled_rope because the coordination core is genuine; the metrics describe heavy asymmetric transfer riding on it.
 *
 * PERSPECTIVAL GAP:
 *   From the commander-treasurer seat the arrangement computes as lawful machinery they operate; from the fighter's seat as duty plus pay; from the unsubmitted community's seat as an ultimatum with no exit that preserves what they are; from the dhimmi's seat as taxation under a protection contract they cannot renegotiate; from the dissenting reader's seat as a tradition they belong to turning on them. Same text, same doctrine — the seats should compute sharply different types because power, exit, and structural position differ radically across parties who nominally share a framework.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (command leadership, fighters, garrison settlers, jurist establishment) drive those seats toward the beneficiary end of d; the victim declarations (unsubmitted communities, treaty allies, dhimmi taxpayers, dissenting readers) drive theirs toward the target end. Exit modulation orders the target seats: trapped populations (unsubmitted communities, dhimmis) sit nearer full-target than the constrained treaty allies, whose security depends on executive restraint they cannot compel. Identity lock orders the insider seats: the jurist establishment's authority is constituted by the framework it administers, and the dissenting reader's learning, language, and audience are all inside the tradition they dispute — both carry their positions structurally rather than by choice, though with opposite sign. The dhimmi seat is genuinely dual — taxed yet shielded from the warfare the doctrine licenses against the unsubmitted — so its effective position sits short of full-target despite the payer role. No directionality overrides were needed: the beneficiary/victim declarations plus the exit atoms already yield the correct ordering.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing the Medinan polity against treaty-breaking coalitions during the peninsular consolidation — was resolved within decades of the verse's revelation. The arrangement avoids mandatrophy by categorical re-description: the reading redescribes the problem as the perennial existence of unsubmitted polytheism, making the mandate unfalsifiable by success and unresolvable by completion. The classification matters because the arrangement is neither pure coordination nor mere predation in religious dress: its mobilization, command, and distribution functions are real and load-bearing for the polity that holds it, while its transfers are existential and asymmetric. Reading it as pure coordination would launder the transfers; reading it as pure predation would misdescribe the machinery that makes the transfers possible and would misdirect remedies — dissolving the coordination function returns nothing that was transferred, and honoring the coordination function justifies nothing that is taken.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates one reading (abrogating_universal) of the contested kernel quran_9_5_scope; what would the sibling readings change structurally?',
    'The sibling stories (quran_9_5_scope__contextual_defensive, quran_9_5_scope__progressive_synthesis) author their own epsilon, victim sets, and classifications; comparing the three files locates the disagreement.',
    'Under the contextual-defensive reading the victim set shrinks to treaty-breaking seventh-century tribes and the standing-obligation structure disappears; under the progressive-synthesis reading the arrangement is time-bound and the present-day victim set empties. This file''s high-extraction profile belongs to the abrogating reading alone and must not be averaged across the family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame marker: one reading of a contested kernel; sibling readings alter the victim set and the standing-obligation structure.').

omega_variable(
    abrogation_transmission_validity,
    'Do the transmitted reports cited to establish the abrogation of the peaceful verses meet the authenticity standards applied in hadith criticism?',
    'Isnad and matn analysis by hadith specialists independent of the reading''s beneficiary set, applied to the sword-verse compilation reports and the fighting-command reports.',
    'If the transmission fails criticism, the reading loses its textual warrant and collapses toward the occasion-bound reading; if it survives, the abrogating structure stands on firmer ground than its critics allow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogation_transmission_validity, empirical, 'Whether the evidentiary chain for universal abrogation withstands standard transmission criticism.').

omega_variable(
    naskh_temporal_vs_categorical,
    'Does abrogation operate on time-bound rulings, whose occasions expire, or does it create categorical obligations that persist after their occasion passes?',
    'Usul al-fiqh analysis of how the abrogating corpus treats expired rulings elsewhere in the tradition, and of whether the tradition itself accepted occasion-expiry for parallel cases.',
    'A temporal reading makes the obligation lapse with its seventh-century occasion, dating the arrangement toward inertial, theatrically maintained persistence; a categorical reading sustains the standing structure indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naskh_temporal_vs_categorical, conceptual, 'Whether the abrogative force is occasion-bound or perpetual — the pivot on which the arrangement''s present-day operation turns.').

omega_variable(
    beneficiary_concentration,
    'Do the arrangement''s gains concentrate in the command, treasury, and settler seats, or diffuse across the believing community as broadly shared spiritual credit?',
    'Fiscal records of spoils division, treasury fifths, and land-grant distribution across periods; comparison of declared distributive rules against realized flows.',
    'Concentration supports reading the arrangement as coordination captured by a narrow set of seats; diffusion supports a broader coordination framing with incidental asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_concentration, empirical, 'Whether material gains pool at identifiable seats or spread across the participant community.').

omega_variable(
    present_operativity_share,
    'What share of jurisdictions and movements holding this reading treat the obligation as presently operative, versus suspended for lack of capacity or prudential deferment?',
    'Survey of contemporary fatwa corpora, court practice, and movement behavior in territories and diasporas where the reading is professed.',
    'Sets the current row of the extractiveness series; a low operativity share pushes the present-day arrangement toward performative maintenance, a high share keeps it actively extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(present_operativity_share, empirical, 'How much of the professed adherence translates into present-day operation.').

omega_variable(
    dissent_suppression_entailment,
    'Is the suppression of internal dissenting readers entailed by the abrogating premise itself, or is it a political accretion that varies across regimes?',
    'Compare the handling of occasion-bound and ethics-trajectory readers across polities and movements holding the abrogating reading at different intensities and different state capacities.',
    'If accretion, the suppression layer is separable and removable without abandoning the doctrine; if entailed, the doctrine and its dissent-suppression stand or fall together, raising the floor under the suppression metric.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dissent_suppression_entailment, conceptual, 'Whether takfir-level suppression of rival readers is structural to the reading or contingent on its holders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__abrogating_universal, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__abrogating_universal, theater_ratio, 0, 0.08).
narrative_ontology:measurement(qura_tr_t200, quran_9_5_scope__abrogating_universal, theater_ratio, 200, 0.12).
narrative_ontology:measurement(qura_tr_t400, quran_9_5_scope__abrogating_universal, theater_ratio, 400, 0.22).
narrative_ontology:measurement(qura_tr_t600, quran_9_5_scope__abrogating_universal, theater_ratio, 600, 0.29).
narrative_ontology:measurement(qura_tr_t800, quran_9_5_scope__abrogating_universal, theater_ratio, 800, 0.33).
narrative_ontology:measurement(qura_tr_t1000, quran_9_5_scope__abrogating_universal, theater_ratio, 1000, 0.27).
narrative_ontology:measurement(qura_tr_t1200, quran_9_5_scope__abrogating_universal, theater_ratio, 1200, 0.56).
narrative_ontology:measurement(qura_tr_t1400, quran_9_5_scope__abrogating_universal, theater_ratio, 1400, 0.41).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__abrogating_universal, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(qura_be_t200, quran_9_5_scope__abrogating_universal, base_extractiveness, 200, 0.86).
narrative_ontology:measurement(qura_be_t400, quran_9_5_scope__abrogating_universal, base_extractiveness, 400, 0.72).
narrative_ontology:measurement(qura_be_t600, quran_9_5_scope__abrogating_universal, base_extractiveness, 600, 0.63).
narrative_ontology:measurement(qura_be_t800, quran_9_5_scope__abrogating_universal, base_extractiveness, 800, 0.59).
narrative_ontology:measurement(qura_be_t1000, quran_9_5_scope__abrogating_universal, base_extractiveness, 1000, 0.67).
narrative_ontology:measurement(qura_be_t1200, quran_9_5_scope__abrogating_universal, base_extractiveness, 1200, 0.44).
narrative_ontology:measurement(qura_be_t1400, quran_9_5_scope__abrogating_universal, base_extractiveness, 1400, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__abrogating_universal, suppression_requirement, 0, 0.76).
narrative_ontology:measurement(qura_su_t200, quran_9_5_scope__abrogating_universal, suppression_requirement, 200, 0.79).
narrative_ontology:measurement(qura_su_t400, quran_9_5_scope__abrogating_universal, suppression_requirement, 400, 0.71).
narrative_ontology:measurement(qura_su_t600, quran_9_5_scope__abrogating_universal, suppression_requirement, 600, 0.65).
narrative_ontology:measurement(qura_su_t800, quran_9_5_scope__abrogating_universal, suppression_requirement, 800, 0.6).
narrative_ontology:measurement(qura_su_t1000, quran_9_5_scope__abrogating_universal, suppression_requirement, 1000, 0.63).
narrative_ontology:measurement(qura_su_t1200, quran_9_5_scope__abrogating_universal, suppression_requirement, 1200, 0.5).
narrative_ontology:measurement(qura_su_t1400, quran_9_5_scope__abrogating_universal, suppression_requirement, 1400, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__abrogating_universal, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__contextual_defensive).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__progressive_synthesis).

% DUAL FORMULATION NOTE:
% The colloquial label 'the ruling on verse 9:5' covers three structurally distinct arrangements differing in epsilon: the abrogating-universal arrangement (this file — universal victim set, standing offensive obligation, high extraction), the contextual-defensive arrangement (victim set limited to seventh-century treaty-breaking tribes, negligible present-day extraction), and the progressive-synthesis arrangement (time-bound directive, empty present-day victim set). Each is authored as its own story with its own epsilon, beneficiaries, and victims; the files are linked because the upstream textual and transmissional claims are cited as warrant across the family, so contamination propagates along these edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
