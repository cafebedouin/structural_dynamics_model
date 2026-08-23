% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__orthodox_literal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__orthodox_literal_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: gita_kurukshetra_discourse__orthodox_literal_reading
 *   human_readable: Kurukshetra Discourse — Orthodox Literal Reading (Caste-Duty Mandate and Righteous War)
 *   domain: religious/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   The orthodox literal reading instantiates the Kurukshetra discourse as
 *   binding legislation: Krishna's counsel fixes each person's duty at birth
 *   by varna, renders kshatriya violence performed as that duty morally
 *   clean, and reserves interpretation of the text to the brahmin
 *   transmission line. The standing arrangement under measurement is
 *   therefore a triple structure — hereditary role assignment, legitimated
 *   righteous war, and an interpretive monopoly — enforced across a
 *   continental agrarian civilization for roughly two millennia. Per the
 *   ε-invariance principle this story measures ONLY this reading's
 *   arrangement: the gandhian allegorical reading and the universalist
 *   devotional reading are separate constraints (separate files, linked
 *   through network.affects_constraints) with their own ε, beneficiary/victim
 *   sets, and types. Claim and metrics are independent authored facts:
 *   claimed_type records tangled_rope because the arrangement coordinates a
 *   real civilizational division of labor while extracting asymmetrically
 *   through the same structure; the metrics record the arrangement's actual
 *   operation and were authored without reference to that claim. KEY AGENTS
 *   (by structural relationship): - brahmin_interpreters: agenda-setting
 *   beneficiary (institutional/identity_locked) — runs the interpretive
 *   monopoly, collects dues and precedence - kshatriya_warrior_aristocracy:
 *   dual-positioned beneficiary-payer (powerful/identity_locked) — collects
 *   legitimation, pays in obligatory blood - shudra_servile_castes and
 *   outcaste_dalit_populations: primary targets (powerless/trapped) — bear
 *   hereditary service and exclusion - opposing_forces_in_dharmic_war:
 *   targets of the legitimated violence (organized/trapped) -
 *   vaishya_mercantile_castes: financing payers with partial protection
 *   benefit (moderate/constrained) - renunciant_and_bhakti_dissenters:
 *   excluded voice (moderate/mobile) - hermeneutic_scholars: analytical
 *   observer (analytical/global)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, 0.78).
domain_priors:suppression_score(gita_kurukshetra_discourse__orthodox_literal_reading, 0.74).
domain_priors:theater_ratio(gita_kurukshetra_discourse__orthodox_literal_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__orthodox_literal_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__orthodox_literal_reading, "Kurukshetra Discourse — Orthodox Literal Reading (Caste-Duty Mandate and Righteous War)").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__orthodox_literal_reading, "religious/textual_hermeneutics/ethical_philosophy").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__orthodox_literal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__orthodox_literal_reading, '59bd3cd2-408b-4a13-9100-6ec4848720b6').
narrative_ontology:cs_kernel_codification('59bd3cd2-408b-4a13-9100-6ec4848720b6', fixed_text).
narrative_ontology:cs_authority_grounding('59bd3cd2-408b-4a13-9100-6ec4848720b6', lineage).
narrative_ontology:cs_interpretation_layer_present('59bd3cd2-408b-4a13-9100-6ec4848720b6').
narrative_ontology:cs_reading_relation('59bd3cd2-408b-4a13-9100-6ec4848720b6', gita_kurukshetra_discourse__gandhian_allegorical_reading, forecloses).
narrative_ontology:cs_reading_relation('59bd3cd2-408b-4a13-9100-6ec4848720b6', gita_kurukshetra_discourse__universalist_devotional_reading, influences).
narrative_ontology:cs_axiom('59bd3cd2-408b-4a13-9100-6ec4848720b6', foundational, svadharma_binding_regardless_of_consequence).
narrative_ontology:cs_axiom_status(svadharma_binding_regardless_of_consequence, holdable).
narrative_ontology:cs_axiom_grounding('59bd3cd2-408b-4a13-9100-6ec4848720b6', svadharma_binding_regardless_of_consequence, theological).
narrative_ontology:cs_axiom('59bd3cd2-408b-4a13-9100-6ec4848720b6', foundational, righteous_violence_duty_fulfilled_is_sinless).
narrative_ontology:cs_axiom_status(righteous_violence_duty_fulfilled_is_sinless, holdable).
narrative_ontology:cs_axiom_grounding('59bd3cd2-408b-4a13-9100-6ec4848720b6', righteous_violence_duty_fulfilled_is_sinless, deontological).
narrative_ontology:cs_reference_frame('59bd3cd2-408b-4a13-9100-6ec4848720b6', varna_svadharma_cosmic_ordinance).
narrative_ontology:cs_drift_state('59bd3cd2-408b-4a13-9100-6ec4848720b6', contemporary_constitutional_republic, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('59bd3cd2-408b-4a13-9100-6ec4848720b6', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpreters).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_aristocracy).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, shudra_servile_castes).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, outcaste_dalit_populations).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, opposing_forces_in_dharmic_war).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, vaishya_mercantile_castes).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_aristocracy).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, vaishya_mercantile_castes).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, varna_dharma_doctrine).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, svadharma_supremacy_claim).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, divine_ordinance_of_caste_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Recite, transmit, and adjudicate the text; they alone are authorized to say what the discourse commands, and every disputed question of duty and war returns to their determination. Ritual dues, land grants, and precedence flow to them continuously. Their station is constituted by the arrangement itself — to leave it would be to cease being what the system defines them as, which the framework holds to be impossible.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpreters, agenda_setter,
    institutional, generational, identity_locked, continental).

% Hold ruling and military functions described as divinely assigned, and receive sanction for violence when it is performed as that assignment. They pay in the same coin: they may not refuse a war the interpreters declare dutiful — the text's own dramatic crisis is a warrior trying to refuse and being argued back onto the field — they absorb the battle deaths, and they sit ritually below the interpreter line whose sanction they finance.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_aristocracy, beneficiary,
    powerful, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_aristocracy, payer).

% Finance the order through agriculture, trade, and tribute, and receive in return protected market standing and the order's peace. They may neither bear arms nor officiate rites; movement into the functions of the classes above or below them is closed, and their daughters' marriages police the boundary as effectively as any tribunal.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, vaishya_mercantile_castes, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__orthodox_literal_reading, vaishya_mercantile_castes, beneficiary).

% Assigned lifelong service to the twice-born orders; barred from Vedic study, from arms, and from independent ritual standing. Duty arrives hereditary and non-negotiable, and transgression invites ritual exclusion and economic ruin administered by their own village elders. There is no recognized exit inside the framework — only the hope of better station in a future life, which the framework itself supplies.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, shudra_servile_castes, payer,
    powerless, generational, trapped, continental).

% Placed outside the varna order altogether: assigned polluting labor, segregated residence, denial of temple entry and common wells. The arrangement's boundary-maintenance defines them as its necessary margin — the category that proves the categories. Flight to another kingdom rarely helps, because the order travels with its texts and its interpreters.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, outcaste_dalit_populations, payer,
    powerless, generational, trapped, continental).

% Armies arrayed against a duty-bound host. Once battle is joined, their slaughter is coded as the fulfillment of the enemy's righteousness rather than as loss requiring justification; desertion before the field is dishonor on their own side, and quarter depends on the victor's observance. They die completing someone else's duty.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, opposing_forces_in_dharmic_war, payer,
    organized, immediate, trapped, regional).

% Teachers and movements who read access to liberation as independent of birth — wandering renouncers, and later the devotional saints who sang in vernaculars the interpreters did not control. They dispute the claims that duty is caste-assigned and that killing cleansed by duty is clean. Kept outside the interpretive conversation, they preach at its margins, are periodically persecuted, and are periodically absorbed.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, renunciant_and_bhakti_dissenters, excluded,
    moderate, biographical, mobile, continental).

% Historians of religion and textual scholars who compare what the same verses have been made to command across eras — caste legislation in one century, inner struggle in another, unconditional devotion in a third. They collect no dues and owe no duties under the arrangement; their seat exists to watch the interpretive machinery work.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, hermeneutic_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpreters).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__orthodox_literal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a continental agrarian civilization across generations without centralized administration: hereditary division of labor assigns ritual, governance, commerce, and service; it regulates recourse to violence by making war legitimate only when undertaken as assigned duty under recognized rules; and it stabilizes role expectations, marriage pools, and succession across a thousand polities.
% TRANSFER_FUNCTION: Moves labor and service upward from the serving orders to the twice-born; moves military blood from the warrior class into wars the interpreter class declares dutiful; moves interpretive authority, dues, and first-claim on honor to the brahmin line; and moves legitimation downward from brahmin sanction to kingly rule.
% ABSENT_VOICES: Those whose duty is assigned without consent have no seat in the interpretive conversation: the serving and outcaste orders are objects of the arrangement's rulings, never parties to them. Dissenting renouncers and vernacular devotional teachers stand outside the monopoly that decides what the text commands. The dead of the declared-righteous wars cannot testify at all.
% DISAPPEARANCE_RATIONALE: If the mandate and its enforcement vanished overnight, the varna division of labor, the endogamy regime, the ritual economy funding the interpreter line, and the legitimation structure of kingship would all lose their warrant simultaneously; warfare across the civilization would lose its duty-frame and need re-justification from scratch. Arrangements touching marriage, labor, ritual, and war do not survive that loss quietly.
% FOUNDING_PROBLEM: Stabilizing a large agrarian social order after prolonged upheaval: how to allocate labor and roles across generations without administrative capacity; how to bound and legitimate warfare between armed polities; and how to answer the fighting man's crisis — why kill at all, and why me — which is the discourse's own dramatic occasion.
% FOUNDING_PROBLEM_CORROBORATION: No corroborating source lies inside the benefiting parties alone. Academic indology and the sociology of caste attest that the coordination problems were real while disputing that the hereditary solution was necessary; the tradition's own margins corroborate the shift — Buddhist and Jain polemicists attacked the birth-based assignment from outside, and the vernacular devotional movements attested in song that access to the ultimate did not require the interpreters' license. What no external source attests is that the founding problem still requires THIS solution; that claim is voiced only by the seats the arrangement privileges.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__orthodox_literal_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__orthodox_literal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__orthodox_literal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78 at interval end) because the arrangement's transfers are structural rather than incidental: station, occupation, marriage pool, and ritual standing are all fixed at birth, service flows permanently upward, and killing is converted from crime into duty for one class while remaining crime for its victims. Suppression (0.74) is a raw structural property, unscaled by power or scope: enforcement runs through caste tribunals, ritual exclusion, economic boycott, endogamy policing, and — per the omega on internalization — through duty fused into identity. Theater (0.30) reflects an apparatus whose calendar rites, lifecycle ceremonies, and war-season norms do real coordinative work, while a growing share of activity maintains the hierarchy symbolically as its economic coordination migrates to jati-level and market mechanisms. Accessibility_collapse (0.58) is deliberately below mountain grade: inside the framework, accepting the svadharma doctrine collapses Arjuna's proposed exits (abstention, renunciation) — the text argues them away line by line — but historically the heterodox exits (Buddhist, Jain, later vernacular devotionalism) stayed open, so alternatives never fully collapsed. Resistance (0.48) records two millennia of sustained heterodox challenge, devotional anti-hierarchical currents, and periodic servile revolt: never victorious within the reading's own frame, never extinguished. The measurement series run on one shared six-point grid so every tracked metric is authored at every examined time point; the rising suppression_requirement series traces genuine enforcement-capacity growth (from reliance on textual authority and social opinion toward organized caste panchayats and excommunication machinery), not mere extraction drift. Values are historiographic estimates over a long interval and carry correspondingly wide error bars. One deliberate gap is recorded rather than reconciled: the reading itself asserts the arrangement is divinely ordained — a naturality claim — while emerges_naturally is left unset because the structural fact is construction enforced by identifiable institutions; that gap between the reading's claim and the arrangement's constitution is itself data the engine consumes.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setting seat compute differently from the same verses. From the brahmin seat the arrangement is cosmic order it administers; from the trapped serving and outcaste seats it is a hereditary ceiling with no door; from the opposing-army seat it is the machinery that converts their deaths into someone else's righteousness. The kshatriya seat is the sharpest divergence: beneficiary of legitimation and rule, payer of obligatory combat and ritual subordination — the text's own opening scene is a beneficiary attempting to resign and being argued back. Same-level dynamics differentiate the two elite seats despite comparable global standing: the interpreters run the machine and collect its rents; the warriors fund it and bleed for it, and their recurrent patronage of heterodox challengers is intra-elite contention over that bargain. Identity-lock operates differently at each seat: brahmin identity is constituted by the interpretive function, kshatriya identity by the warrior duty — which is why Arjuna's momentary frame-break triggers the text's full persuasive machinery rather than a simple permission. Coalition potential among the powerless seats existed but was systematically blunted: the arrangement grades the subordinate orders against each other (touchable serving castes above outcastes, ritual rank within villages), fragmenting the very class a coalition would require.
 *
 * DIRECTIONALITY LOGIC:
 *   The brahmin interpreters sit at the full-beneficiary end (d near 0.05–0.10): the arrangement subsidizes them with dues, precedence, and monopoly, and they bear almost none of its costs. The trapped serving and outcaste seats sit near the full-target end (d ≈ 0.85–0.95): they supply the labor and absorb the exclusion with no exit. The opposing armies are near-full targets (d ≈ 0.9) — legitimated violence lands on them directly, and desertion is foreclosed by honor codes on their own side. The vaishya seat sits mid-range (d ≈ 0.55): real payments upward, real protection received. One explicit override is authored: for the powerful power-atom (occupied here by the kshatriya aristocracy), d is set to 0.35. The automatic derivation from the beneficiary declaration alone would place that seat near the beneficiary pole, but the arrangement obligates them to fight and die in wars they do not declare, and subordinates them ritually to the interpreters they fund — the text's own drama exists because that seat tried to exit. The override encodes the dual position the structural declarations imply but the derivation underweights. Excluded dissenters and the analytical observer hold no directional position in the derivation: one is outside the conversation, the other outside the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — allocating labor without administration, bounding war, answering the fighter's crisis — is partially live in transformed forms, but the hereditary-caste solution to it is what stands contested; hence founding_problem_status is contested rather than dead, and no zombie flag fires on the status-by-verdict mismatch. The classification prevents mislabeling in both directions. Reading the arrangement as pure extraction (snare) would erase the genuine coordination function: continental-scale role allocation, war-season regulation, and lifecycle integration that predate markets and bureaucracies and that the civilization did not replace for two millennia. Reading it as pure coordination (rope) would erase the structural asymmetry: the same verses that organize the division of labor lock the serving orders out of mobility and convert slaughter into duty. Tangled rope names both halves honestly. The mandatrophy question for this arrangement is whether the coordination half still requires the extraction half — the separability question the omega on the kshatriya coalition and the reception-history record both bear on — and the corpus should expect that answer to differ across the three sibling readings, which is precisely why they are separate files.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of kernel gita_kurukshetra_discourse — the orthodox_literal_reading. Sibling readings (gandhian_allegorical_reading, universalist_devotional_reading) instantiate different constraints from the same verses. What structurally changes if a sibling reading displaces this one institutionally?',
    'Each reading is authored as its own ε-invariant story and linked by network.affects_constraints; displacement is tracked through reception history and institutional authority shifts rather than by re-measuring this file.',
    'Under either sibling the victim set contracts sharply — no hereditary duty-lock, no legitimated killing, no interpretive-monopoly rents — so ε for the displaced arrangement falls steeply and the payer seats dissolve as categories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: this story is one indexed reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    command_force_disagreement_location,
    'Where exactly do the readings disagree: on the illocutionary force of the discourse''s command — literal legislation binding physical violence on a hereditary class (this reading), allegory of inner struggle (gandhian), or devotional invitation overriding social role (universalist)?',
    'Hermeneutic analysis of the text''s genre signals and imperative passages (1st-person divine command, battlefield setting, Arjuna''s concrete refusal) combined with reception-history evidence of how each community of readers treated the command.',
    'Determines which seats exist at all: this reading seats kshatriyas, shudras, outcastes, and opposing armies; the allegorical reading seats an individual psyche; the devotional reading seats devotees irrespective of birth. Seat-set divergence, not metric disagreement, is the deepest structural difference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(command_force_disagreement_location, conceptual, 'The located axis of kernel contest: the command''s force, not its ethical valence.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of the lower-order seats structural (caste tribunals, ritual exclusion, economic boycott, endogamy policing) or internalized (duty and station fused into identity, so compliance persists where enforcement weakens)?',
    'Post-exit trajectory of communities that left the arrangement (conversion to heterodox traditions, migration, modern legal emancipation): if hierarchy-seeking and duty-internalization persist after enforcement machinery is removed, a substantial share is internalized.',
    'If internalized, effective suppression exceeds the structural measure — the arrangement reproduces itself through the identities of its own targets, raising persistence and lowering measured resistance without any additional coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in caste-duty compliance.').

omega_variable(
    kshatriya_coalition_stability,
    'Does the enforcement coalition hold only while the kshatriya seat''s receipts (legitimated rule, sanctified violence) exceed its costs (obligatory combat, battle deaths, ritual subordination to the interpreters it funds)?',
    'Historical analysis of royal patronage shifts: episodes where kshatriya dynasties funded heterodox challenges (Buddhist and Jain establishments) mark moments the net position flipped; recurrence and duration of such episodes test the coalition''s stability condition.',
    'If the kshatriya net position turns durably negative, the enforcement coalition fractures and the arrangement survives mainly by inertia and performance — drifting the computed classification toward a degraded, administratively maintained form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kshatriya_coalition_stability, empirical, 'Intra-elite tension between the interpreter seat and the warrior seat as the arrangement''s stability condition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__orthodox_literal_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(gita_tr_t20, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(gita_tr_t40, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(gita_tr_t60, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(gita_tr_t80, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 80, 0.27).
narrative_ontology:measurement(gita_tr_t100, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(gita_be_t20, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(gita_be_t40, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(gita_be_t60, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 60, 0.73).
narrative_ontology:measurement(gita_be_t80, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 80, 0.76).
narrative_ontology:measurement(gita_be_t100, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gita_su_t20, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(gita_su_t40, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 40, 0.64).
narrative_ontology:measurement(gita_su_t60, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 60, 0.67).
narrative_ontology:measurement(gita_su_t80, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 80, 0.71).
narrative_ontology:measurement(gita_su_t100, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 100, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__orthodox_literal_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gandhian_allegorical_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Gita's teaching on war and duty' covers three structurally distinct claims and decomposes per the ε-invariance principle into three linked stories. This file (orthodox_literal_reading) is the upstream member: for most of the reception history it is the reading cited as authoritative, and its institutional weight is what the sibling readings define themselves against — the gandhian allegory and the universalist devotional reading each emerged as counter-readings whose legitimacy conditions this reading's monopoly shaped. Each member carries its own ε, beneficiary/victim set, and type; none hedges across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gita_kurukshetra_discourse__orthodox_literal_reading, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
