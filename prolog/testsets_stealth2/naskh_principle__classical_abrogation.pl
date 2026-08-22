% ============================================================================
% CONSTRAINT STORY: naskh_principle__classical_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__classical_abrogation, []).

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
 *   constraint_id: naskh_principle__classical_abrogation
 *   human_readable: Classical Naskh: Chronological Abrogation of Quranic Legal Rulings
 *   domain: religious/legal/hermeneutical
 *
 * SUMMARY:
 *   Classical Sunni legal theory resolved apparent conflicts among Quranic
 *   rulings by chronological supersession: a later revealed ruling replaces
 *   an earlier one on the same topic, with the abrogated verse retaining
 *   recitative and spiritual value but losing binding legal force. Formalized
 *   from the 8th century CE onward and disciplined by evidentiary criteria
 *   (explicit text or clear indication, reliably transmitted reports, genuine
 *   irreconcilability), the doctrine gave courts a determinate answer to
 *   'which ruling governs now' and gave the juristic class a curricular
 *   spine. Its costs fell on those whose readings it voided, on lay believers
 *   asked to hold that divine speech contains self-replaced legislation, and
 *   — where the sword-verse abrogation was accepted — on non-Muslim subjects
 *   whose legal terms hardened. This file is ONE reading of the
 *   naskh_principle kernel (see kernel_context and omegas); the sibling
 *   readings are separate constraints with their own epsilon values, not
 *   hedges inside this one. KEY AGENTS (by structural relationship): -
 *   classical_madhab_establishment: Agenda-setter and principal beneficiary
 *   (institutional/identity_locked) — codifies the criteria, maintains the
 *   abrogating/abrogated lists, and constitutes its own scholarly authority
 *   through mastery of this corpus - sharia_court_judges: Beneficiary
 *   (institutional/constrained) — purchases determinate, defensible rulings
 *   without re-litigating textual conflict - contextualist_interpreters:
 *   Primary target (moderate/constrained) — harmonizing readings carry no
 *   legal force and attract sanction risk - devout_lay_believers:
 *   Dual-positioned bearer (organized/constrained) — receives clear practice
 *   rules, carries the theological-coherence cost -
 *   dhimmi_non_muslim_subjects: Hardest-target class (powerless/trapped) —
 *   legal status fixed by a conversation they had no seat in -
 *   modernist_reform_scholars: Excluded voice (organized/constrained) —
 *   contests the mechanism from outside consensus formation -
 *   academic_islamicists: Analytical observer (institutional/analytical) —
 *   maps chronology and counts abrogation claims without adjudicating
 *   religious authority
 *
 * KEY AGENTS:
 *   - classical_madhab_establishment: agenda-setter and principal beneficiary; identity_locked — its authority is constituted by the corpus it administers
 *   - sharia_court_judges: beneficiary; constrained exit — determinate rulings are the purchased good
 *   - contextualist_interpreters: primary target; constrained exit — readings voided, curricular access gated
 *   - devout_lay_believers: dual-positioned (payer + beneficiary); constrained exit — clear practice rules received, coherence cost carried
 *   - dhimmi_non_muslim_subjects: hardest-target class; trapped — status fixed without a seat
 *   - modernist_reform_scholars: excluded voice; organized but outside consensus formation
 *   - academic_islamicists: analytical observer; describes the structure without wielding its authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__classical_abrogation, 0.62).
domain_priors:suppression_score(naskh_principle__classical_abrogation, 0.32).
domain_priors:theater_ratio(naskh_principle__classical_abrogation, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, extractiveness, 0.62).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__classical_abrogation, tangled_rope).
narrative_ontology:human_readable(naskh_principle__classical_abrogation, "Classical Naskh: Chronological Abrogation of Quranic Legal Rulings").
narrative_ontology:topic_domain(naskh_principle__classical_abrogation, "religious/legal/hermeneutical").

domain_priors:requires_active_enforcement(naskh_principle__classical_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__classical_abrogation, '85814875-33e9-4c95-8d57-f608c1ce0a62').
narrative_ontology:cs_kernel_codification('85814875-33e9-4c95-8d57-f608c1ce0a62', fixed_text).
narrative_ontology:cs_authority_grounding('85814875-33e9-4c95-8d57-f608c1ce0a62', lineage).
narrative_ontology:cs_interpretation_layer_present('85814875-33e9-4c95-8d57-f608c1ce0a62').
narrative_ontology:cs_reading_relation('85814875-33e9-4c95-8d57-f608c1ce0a62', naskh_principle__contextual_harmonization, coexists_with).
narrative_ontology:cs_reading_relation('85814875-33e9-4c95-8d57-f608c1ce0a62', naskh_principle__progressive_restriction, influences).
narrative_ontology:cs_axiom('85814875-33e9-4c95-8d57-f608c1ce0a62', foundational, later_revelation_voids_earlier_legal_force).
narrative_ontology:cs_axiom_status(later_revelation_voids_earlier_legal_force, holdable).
narrative_ontology:cs_axiom_grounding('85814875-33e9-4c95-8d57-f608c1ce0a62', later_revelation_voids_earlier_legal_force, theological).
narrative_ontology:cs_axiom('85814875-33e9-4c95-8d57-f608c1ce0a62', secondary, abrogation_requires_explicit_text_or_clear_indication).
narrative_ontology:cs_axiom_status(abrogation_requires_explicit_text_or_clear_indication, holdable).
narrative_ontology:cs_axiom_grounding('85814875-33e9-4c95-8d57-f608c1ce0a62', abrogation_requires_explicit_text_or_clear_indication, conventional).
narrative_ontology:cs_reference_frame('85814875-33e9-4c95-8d57-f608c1ce0a62', chronological_supersession_hierarchy).
narrative_ontology:cs_drift_state('85814875-33e9-4c95-8d57-f608c1ce0a62', post_modernist_critique, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('85814875-33e9-4c95-8d57-f608c1ce0a62', '').
narrative_ontology:cs_kernel_id(naskh_principle__classical_abrogation, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, classical_madhab_establishment).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, sharia_court_judges).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, contextualist_interpreters).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, devout_lay_believers).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, dhimmi_non_muslim_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, devout_lay_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Codifies the criteria for supersession (explicit textual indication, reliably transmitted reports, genuine irreconcilable conflict), maintains the lists of abrogating and abrogated verses, trains jurists through curricula organized around the chronological hierarchy, staffs courts and mufti posts, and issues the rulings that cite it. Its scholarly standing is constituted by mastery of this corpus: renouncing the framework would dissolve the basis of its own authority, so exit looks like professional self-erasure. Collects interpretive authority, institutional position, and curricular control directly.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, classical_madhab_establishment, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__classical_abrogation, classical_madhab_establishment, beneficiary).

% Adjudicate cases using settled rulings without re-opening textual conflicts in every proceeding: the hierarchy tells them which verse governs now, making judgments fast, uniform, and defensible on appeal. Leaving the bench or the madhab framework that certifies them costs career and standing, so they operate inside the arrangement they profit from.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, sharia_court_judges, beneficiary,
    institutional, biographical, constrained, regional).

% Scholars who read the disputed verse-pairs as context-specific — each verse valid within its occasion of revelation — rather than chronologically voided. Under the operative doctrine their readings carry no legal force, are excluded from official curricula and court methodology, and can attract charges of undermining divine law; the 1995 apostasy ruling against Nasr Abu Zayd followed partly from his hermeneutical approach to exactly this material. Exit means publishing outside religious institutions, losing audience and standing, or recanting.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, contextualist_interpreters, payer,
    moderate, generational, constrained, continental).

% Receive a clear answer to 'what governs now' for worship, inheritance, and conduct — a genuine everyday benefit. They also carry the coherence cost: holding that God's speech contains rulings He later replaced sits uneasily with the Quran's self-description as a book whose verses confirm one another (Q 4:82), a tension the tradition manages but does not dissolve. Leaving the believing community carries existential social cost, so their deference funds the establishment's authority while the tension stays theirs to hold.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, devout_lay_believers, payer,
    organized, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__classical_abrogation, devout_lay_believers, beneficiary).

% Where the sword-verse abrogation (Q 9:5 over the tolerance and treaty verses) was accepted, the legal terms of non-Muslim subjection hardened: treaty flexibility narrowed, tribute regimes entrenched, and intercommunal relations were governed by the later, stricter rulings. They had no seat in the hermeneutical conversation that fixed their status and no practical exit from the jurisdictions applying it. Historically concentrated in the classical-to-early-modern periods; the legacy surfaces whenever revived jurisprudence movements revisit the same abrogation claims.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, dhimmi_non_muslim_subjects, payer,
    powerless, generational, trapped, continental).

% From Muhammad Abduh onward, argue that the classical abrogation lists are inflated and that the mechanism itself misreads progressive revelation; they press for contextual specification in its place. Their critiques circulate widely in print and online but rarely enter official seminary curricula, court methodology, or fatwa-body procedure — they contest from outside the rooms where the operative account is maintained.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, modernist_reform_scholars, excluded,
    organized, generational, constrained, global).

% University-based scholars, Muslim and non-Muslim, who reconstruct revelation chronology, inventory and count abrogation claims across the tradition, and document how the doctrine's scope expanded and contracted. They describe the structure and its history without wielding or submitting to its religious authority.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, academic_islamicists, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__classical_abrogation, classical_madhab_establishment).
narrative_ontology:fixing_cost_class(naskh_principle__classical_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts a scripture containing chronologically dispersed, apparently conflicting rulings into a single operative legal code: the supersession hierarchy tells judges, schools, and believers which ruling binds now, so apparent conflicts are resolved once, centrally, by established criteria instead of ad hoc in every case or classroom.
% TRANSFER_FUNCTION: Moves interpretive authority from individual reasoners and rival hermeneutical schools to the transmission-certified juristic establishment; moves legal certainty and adjudicative speed to courts and lay practice; moves the costs of textual rigidity to contextualist interpreters (voided readings, gated access), to lay believers (theological-coherence burden), and — where the sword-verse ruling was accepted — to non-Muslim subjects (hardened legal status).
% ABSENT_VOICES: Rationalist and contextualist hermeneuts were largely outside the consensus formations that fixed the abrogation lists — Mu'tazila-leaning exegetes historically, reformist scholars modernly — so the unanimity of the classical lists partly reflects who was admitted to form it. Lay believers were never consulted on the coherence cost assigned to them. Non-Muslim subjects had no seat at all in the jurisprudence that determined their legal terms.
% DISAPPEARANCE_RATIONALE: Islamic legal education, court methodology, and commentary literature are organized around the supersession hierarchy. Overnight removal would reopen every settled ruling the doctrine touches — inheritance shares, prayer direction, the staged prohibitions, intercommunal law — force the schools to rebuild adjudication from raw textual conflict, and dissolve the curricular spine on which the juristic class's authority and the judiciary's determinacy both rest.
% FOUNDING_PROBLEM: Early Muslim communities received a scripture whose rulings arrived piecemeal over roughly two decades and sometimes diverged — wine, prayer direction, inheritance shares, conduct of warfare. Believers and judges needed to know which ruling governed now, and the community needed the divergence reconciled with the doctrine that the speech was divine and internally consistent.
% FOUNDING_PROBLEM_CORROBORATION: Philological and chronological scholarship outside the beneficiary set — academic quranic studies, Muslim and non-Muslim — corroborates the underlying textual phenomenon: rulings that vary across revelation periods are observably there. But no external party attests that chronological supersession specifically is the required resolution: reformist scholars inside the tradition dispute that the founding problem survives as stated (arguing the 'contradictions' were contextual specifications all along), and comparative historians note sibling scriptural traditions managed progressive strata without invalidation. The liveness of the problem-as-requiring-this-solution rests principally with the benefiting establishment; the phenomenon itself is externally corroborated, the mandated remedy is not.
narrative_ontology:disappearance_verdict(naskh_principle__classical_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__classical_abrogation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__classical_abrogation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(naskh_principle__classical_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__classical_abrogation, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__classical_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__classical_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__classical_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.62 at interval end) because the doctrine's yield to its administrators is decoupled from adjudicative necessity: large portions of the classical lists served doctrinal tidiness and institutional authority more than any courtroom need, and the sword-verse application withdrew legal flexibility from an entire class of subjects who never entered the hermeneutical conversation. Suppression (0.32, end-state) is epistemic rather than physical — curriculum control, gatekeeping of official posts, and chargeable deviance (the Abu Zayd proceedings show the machinery can still fire) — and the temporal series traces its build-up through madhab consolidation and decay through modernity. Theater (0.38) rises over the interval: abrogated verses are still recited ritually while denied legal force, and a growing share of contemporary activity is curricular reproduction defending the hierarchy rather than solving live adjudication problems. Accessibility_collapse is moderate (0.50): inside the framework alternatives collapse, but classical usul always ran specification alongside abrogation, and exit to contextualist hermeneutics remains thinkable. Resistance (0.60) is real and old — rationalist objections, the modernist reduction of lists from hundreds toward single digits, and the backlash that persecution of dissenters itself provoked. The claimed type (tangled_rope) is authored from structure — genuine coordination function, asymmetric extraction, active enforcement — independently of these metric values; the engine computes per-seat classifications from the structural data, and any divergence between claim and computed output is the measurement, not an error to reconcile.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute very differently. From the judge's seat the arrangement is close to pure coordination: determinacy is a real, daily-used good and the judge bears little of the cost. From the establishment's seat it is coordination constitutive of identity — the corpus IS the authority, so no vantage from inside can price the extraction it imposes on outsiders. From the contextualist interpreter's seat the same structure operates as enforced exclusion: their readings are voided by criteria they may reject, with career and standing at stake. From the dhimmi subject's seat it operated as unilateral status-fixation with no compensating benefit and no exit. The lay believer's seat is genuinely mixed — clear practice rules received, coherence cost carried. The engine computes this divergence from power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The establishment sits nearest the beneficiary end (collects authority and institutional rents; identity_lock amplifies its captured position). Judges sit low-d: net beneficiaries with constrained but real alternatives (bench, madhab affiliation). Contextualist interpreters sit high-d: they pay in voided readings and gated access, and constrained exit traps them near the target end. Dhimmi subjects sit at the extreme target end: powerless, trapped, and scoped to the jurisdictions where the hardened rulings applied. Devout lay believers derive mid-range: the dual beneficiary/payer declaration plus constrained (not trapped) exit places them near symmetric, which matches the phenomenology — they receive the coordination good and fund the coherence cost. Modernist reform scholars are excluded rather than seated: their absence feeds the consensus-provenance check (Q4), not directionality. Suppression is authored as a raw structural property and is deliberately NOT scaled; only extractiveness is scaled, by directionality and by the global scope of the curricular and juridical network, which raises verification difficulty and thus effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification blocks the two available mislabels. Reading the doctrine as pure coordination (rope) erases the suppressed classes: the voided interpreters, the coherence cost pushed onto believers, and the hardened intercommunal law — all structural facts, not rhetorical complaints. Reading it as pure extraction (snare) erases twelve centuries of genuine adjudicative service: courts confronting a scripture with chronologically dispersed rulings did need a resolution procedure, and the procedure worked. The mandatrophy watchpoint is scope-atrophy rather than dead mandate: the maximal classical apparatus (two-hundred-item lists) has shrunk dramatically even where the mechanism remains live, so the residue trends toward theatrical maintenance in purely curricular settings while staying functional wherever courts still apply it. The R5 interview records founding_problem_status=contested with disappearance_verdict=world_rearranges — no dead-mandate mismatch fires, correctly, because the core function persists even as the parties dispute whether the founding problem survives in a form requiring this solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates the classical_abrogation reading of the naskh_principle kernel; what structurally changes if a sibling reading (contextual_harmonization or progressive_restriction) displaces it as the operative account?',
    'Track which reading governs official curricula, court methodology, and fatwa bodies across major institutions. Displacement appears as abrogation lists shrinking to zero (contextual harmonization) or as superseded rulings being recharacterized as stage-specific pedagogy rather than invalidated law (progressive restriction).',
    'Under contextual_harmonization the payer class of excluded contextualist interpreters dissolves and measured extraction falls toward coordination-floor levels. Under progressive_restriction the supersession hierarchy survives formally but abrogated verses regain contextual validity, shrinking the extraction surface without changing the beneficiary structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story is one of three readings of the naskh kernel; sibling adoption would redraw the victim set and epsilon.').

omega_variable(
    abrogation_corpus_scope,
    'How many verse-pairs constitute genuine abrogation under the classical criteria?',
    'Philological audit against the classical gates (explicit textual indication, mass-transmitted reports, genuinely irreconcilable conflict). The historical arc runs from 200+-item lists in some Shafi''i-era compilations, through Suyuti''s narrowing, to modern counts below ten.',
    'Scope sets the doctrine''s extraction footprint: a minimal list leaves a small coordination core with little asymmetry; a maximal list extends the hierarchy across intercommunal law, inheritance, and worship, widening both the coordinated surface and the extracted surface.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogation_corpus_scope, empirical, 'The size of the operative abrogation list is contested by two orders of magnitude across the tradition.').

omega_variable(
    sword_verse_abrogation_validity,
    'Does Q 9:5 abrogate the tolerance and treaty verses (e.g., 2:109, 8:61, 60:8), as substantial portions of the classical tradition held?',
    'Chronological and occasions-of-revelation analysis: determine whether the earlier peace-and-tolerance verses were context-bound terms for specific treaties rather than permanently superseded general legislation.',
    'Affirming it hardens the doctrine''s most consequential application — the legal terms governing non-Muslims — and anchors maximal-list readings. Denying it removes the single highest-stakes abrogation claim and strengthens contextualist accounts of the same material.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sword_verse_abrogation_validity, empirical, 'Validity of the highest-stakes single abrogation claim in the corpus.').

omega_variable(
    naskh_necessity_vs_construction,
    'Is chronological supersession a necessary feature of any sequentially revealed, evolving legislation, or a constructed juristic choice that alternative hermeneutics could replace wholesale?',
    'Comparative analysis of scripture-based legal systems handling internal textual development — e.g., rabbinic harmonization of biblical law''s progressive strata without invalidation — to test whether supersession is forced by the structure of sequential revelation or selected by juristic preference.',
    'If structurally necessary, part of the measured extraction is the unavoidable price of the coordination itself and the coordination component strengthens. If constructed, the doctrine''s persistence reflects institutional maintenance rather than inevitability, strengthening the extraction-side reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naskh_necessity_vs_construction, conceptual, 'Whether the mechanism is structurally forced or institutionally chosen.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__classical_abrogation, 820, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t820, naskh_principle__classical_abrogation, theater_ratio, 820, 0.1).
narrative_ontology:measurement(nask_tr_t1100, naskh_principle__classical_abrogation, theater_ratio, 1100, 0.13).
narrative_ontology:measurement(nask_tr_t1350, naskh_principle__classical_abrogation, theater_ratio, 1350, 0.17).
narrative_ontology:measurement(nask_tr_t1600, naskh_principle__classical_abrogation, theater_ratio, 1600, 0.21).
narrative_ontology:measurement(nask_tr_t1850, naskh_principle__classical_abrogation, theater_ratio, 1850, 0.26).
narrative_ontology:measurement(nask_tr_t1925, naskh_principle__classical_abrogation, theater_ratio, 1925, 0.31).
narrative_ontology:measurement(nask_tr_t1975, naskh_principle__classical_abrogation, theater_ratio, 1975, 0.35).
narrative_ontology:measurement(nask_tr_t2026, naskh_principle__classical_abrogation, theater_ratio, 2026, 0.38).

% Extraction over time
narrative_ontology:measurement(nask_be_t820, naskh_principle__classical_abrogation, base_extractiveness, 820, 0.4).
narrative_ontology:measurement(nask_be_t1100, naskh_principle__classical_abrogation, base_extractiveness, 1100, 0.52).
narrative_ontology:measurement(nask_be_t1350, naskh_principle__classical_abrogation, base_extractiveness, 1350, 0.63).
narrative_ontology:measurement(nask_be_t1600, naskh_principle__classical_abrogation, base_extractiveness, 1600, 0.67).
narrative_ontology:measurement(nask_be_t1850, naskh_principle__classical_abrogation, base_extractiveness, 1850, 0.7).
narrative_ontology:measurement(nask_be_t1925, naskh_principle__classical_abrogation, base_extractiveness, 1925, 0.66).
narrative_ontology:measurement(nask_be_t1975, naskh_principle__classical_abrogation, base_extractiveness, 1975, 0.64).
narrative_ontology:measurement(nask_be_t2026, naskh_principle__classical_abrogation, base_extractiveness, 2026, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t820, naskh_principle__classical_abrogation, suppression_requirement, 820, 0.3).
narrative_ontology:measurement(nask_su_t1100, naskh_principle__classical_abrogation, suppression_requirement, 1100, 0.46).
narrative_ontology:measurement(nask_su_t1350, naskh_principle__classical_abrogation, suppression_requirement, 1350, 0.58).
narrative_ontology:measurement(nask_su_t1600, naskh_principle__classical_abrogation, suppression_requirement, 1600, 0.62).
narrative_ontology:measurement(nask_su_t1850, naskh_principle__classical_abrogation, suppression_requirement, 1850, 0.56).
narrative_ontology:measurement(nask_su_t1925, naskh_principle__classical_abrogation, suppression_requirement, 1925, 0.46).
narrative_ontology:measurement(nask_su_t1975, naskh_principle__classical_abrogation, suppression_requirement, 1975, 0.38).
narrative_ontology:measurement(nask_su_t2026, naskh_principle__classical_abrogation, suppression_requirement, 2026, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__classical_abrogation, enforcement_mechanism).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__contextual_harmonization).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__progressive_restriction).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'naskh' covers three structurally distinct claims about how sequential revelation resolves internal legal conflict, and forcing them into one story would make epsilon observer-dependent. This story authors the classical_abrogation claim (epsilon 0.62 for the standing classical arrangement). The sibling files author contextual_harmonization and progressive_restriction with their own metrics and stakeholders. Edges run FROM this story TO both siblings because the classical reading is upstream: its compiled revelation chronology and its inventoried verse-pairs are the raw material the other two readings reinterpret — contextual harmonization answers the maximal lists, and progressive restriction accepts the chronology while recharacterizing it. Upstream empirical confidence is higher here (the chronological ordering itself is broadly conceded); the downstream readings are the contested reconstructions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
