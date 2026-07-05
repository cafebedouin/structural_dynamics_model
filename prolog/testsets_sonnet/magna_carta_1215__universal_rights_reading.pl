% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__universal_rights_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: magna_carta_1215__universal_rights_reading
 *   human_readable: Magna Carta Clause 39 as Universal Due Process Precedent
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This story instantiates the universal-rights reading of the Magna Carta
 *   kernel: the claim that Clause 39's phrase 'no free man' has always meant,
 *   or has come to legitimately mean, all persons subject to state coercive
 *   power — making the 1215 charter a transhistorical precedent for universal
 *   due process rather than a settlement limited to feudal barons. This is
 *   one of three readings of the same kernel text; the
 *   baronial_privilege_reading holds that 'free men' meant landowning barons
 *   only, and the living_document_reading holds that original meaning is
 *   legitimately superseded by interpretive accumulation regardless of what
 *   the original scope was. This story does not adjudicate between them — it
 *   authors the universal-rights reading cleanly, on its own ε.
 *
 * KEY AGENTS:
 *   - criminal_defendants: primary beneficiary (powerless/trapped) — invoke the reading for procedural protection
 *   - detained_persons: primary beneficiary (powerless/trapped) — the class whose inclusion is most contested
 *   - civil_liberties_litigants: agenda-setting beneficiary (organized/mobile) — actively construct and press the reading
 *   - constitutional_courts: agenda_setter (institutional/analytical) — administer and extend the doctrine through case law
 *   - executive_detention_authorities: primary payer (institutional/constrained) — bear compliance costs
 *   - summary_punishment_regimes: payer (powerful/constrained) — find extrajudicial action barred
 *   - legal_historians: analytical observer — document the gap between 1215 text and modern reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__universal_rights_reading, 0.28).
domain_priors:suppression_score(magna_carta_1215__universal_rights_reading, 0.22).
domain_priors:theater_ratio(magna_carta_1215__universal_rights_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__universal_rights_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__universal_rights_reading, "Magna Carta Clause 39 as Universal Due Process Precedent").
narrative_ontology:topic_domain(magna_carta_1215__universal_rights_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__universal_rights_reading, '16d2c726-75a7-43fd-92dd-bb80d7f10915').
narrative_ontology:cs_kernel_codification('16d2c726-75a7-43fd-92dd-bb80d7f10915', fixed_text).
narrative_ontology:cs_authority_grounding('16d2c726-75a7-43fd-92dd-bb80d7f10915', lineage).
narrative_ontology:cs_interpretation_layer_present('16d2c726-75a7-43fd-92dd-bb80d7f10915').
narrative_ontology:cs_reading_relation('16d2c726-75a7-43fd-92dd-bb80d7f10915', magna_carta_1215__baronial_privilege_reading, forecloses).
narrative_ontology:cs_reading_relation('16d2c726-75a7-43fd-92dd-bb80d7f10915', magna_carta_1215__living_document_reading, influences).
narrative_ontology:cs_axiom('16d2c726-75a7-43fd-92dd-bb80d7f10915', foundational, liber_homo_denotes_all_persons_under_state_power).
narrative_ontology:cs_axiom_status(liber_homo_denotes_all_persons_under_state_power, holdable).
narrative_ontology:cs_axiom_grounding('16d2c726-75a7-43fd-92dd-bb80d7f10915', liber_homo_denotes_all_persons_under_state_power, deontological).
narrative_ontology:cs_axiom('16d2c726-75a7-43fd-92dd-bb80d7f10915', secondary, clause_39_protection_scope_is_class_independent).
narrative_ontology:cs_axiom_status(clause_39_protection_scope_is_class_independent, holdable).
narrative_ontology:cs_axiom_grounding('16d2c726-75a7-43fd-92dd-bb80d7f10915', clause_39_protection_scope_is_class_independent, conventional).
narrative_ontology:cs_reference_frame('16d2c726-75a7-43fd-92dd-bb80d7f10915', baronial_feudal_settlement_1215).
narrative_ontology:cs_drift_state('16d2c726-75a7-43fd-92dd-bb80d7f10915', post_universal_human_rights_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('16d2c726-75a7-43fd-92dd-bb80d7f10915', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__universal_rights_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, criminal_defendants).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, detained_persons).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, civil_liberties_litigants).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, constitutional_courts).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, executive_detention_authorities).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, summary_punishment_regimes).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, universal_due_process_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, rule_of_law_supremacy_over_executive_will).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals facing state prosecution who invoke the Clause 39 lineage — 'no free man shall be... imprisoned... except by lawful judgment of his peers or by the law of the land' — as ancestral authority for procedural protections against arbitrary detention. They have no exit from the criminal process itself; the constraint's value to them is entirely in constraining how the state may act against them.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, criminal_defendants, beneficiary,
    powerless, biographical, trapped, national).

% Persons held by state authority (including non-citizens, foreign nationals, and stateless individuals) who rely on the universal reading to claim that Clause 39's protection was never limited to a feudal class and extends to any person subject to a state's coercive power. Their situation is often acute and time-sensitive; the constraint's reach to them is precisely the contested point.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, detained_persons, beneficiary,
    powerless, immediate, trapped, global).

% Advocacy organizations and litigators who actively construct and press the universal-rights reading in courts, treating it as a foundational precedent for due process and habeas corpus doctrine worldwide. They have professional and institutional mobility across jurisdictions and actively maintain the reading through litigation strategy.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, civil_liberties_litigants, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__universal_rights_reading, civil_liberties_litigants, agenda_setter).

% Judicial bodies (from English common law courts through the U.S. Supreme Court to international human rights tribunals) that cite Clause 39 as transhistorical authority when adjudicating due process claims. They administer and extend the reading through case law, treating 'free men' as a term whose scope has always logically included all persons even if not historically enforced that way.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, constitutional_courts, agenda_setter,
    institutional, civilizational, analytical, global).

% State executive branches, military authorities, and law enforcement agencies whose detention and punishment powers are constrained by the universal reading's insistence that due process obligations run to all persons, not merely to a narrow enfranchised class. They bear the compliance cost — procedural requirements, judicial review, habeas litigation — and their exit is constrained by the doctrine's entrenchment in constitutional and international law.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, executive_detention_authorities, payer,
    institutional, immediate, constrained, national).

% Governments or administrative bodies seeking to impose extrajudicial or summary sanctions (administrative detention, emergency powers, expedited deportation) who find the universal reading invoked against them as a bar to bypassing judicial process for any person, not just citizens or property-holders. They experience the constraint as a persistent legal obstacle that must be litigated around or formally suspended.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, summary_punishment_regimes, payer,
    powerful, biographical, constrained, national).

% Scholars who examine the 1215 text and its baronial context, noting that 'liber homo' (free man) in the original charter denoted a specific class of free tenants and excluded villeins, serfs, and women in the contemporary legal sense — and who document how the universal reading emerged centuries later through selective quotation (notably by Coke) rather than from the document's own terms.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% Villeins, serfs, and other unfree persons under the 1215 charter were categorically outside 'liber homo' and had no voice in the charter's negotiation or its later reinterpretation. They are not living stakeholders today, but their historical exclusion is structurally relevant: the universal reading retrofits inclusion the original document did not grant, and no one speaks for them as a corroborating source for the founding claim.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, medieval_unfree_classes_analogue, excluded,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(magna_carta_1215__universal_rights_reading, medieval_unfree_classes_analogue).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, citable textual anchor that different legal systems and courts can converge on when constructing due process protections — a common ancestral reference point that lets otherwise disparate common-law and constitutional traditions claim continuity and legitimacy for procedural rights against arbitrary state power.
% TRANSFER_FUNCTION: Moves legitimating authority from a 1215 baronial settlement to modern claims of universal procedural protection; in practical effect, it transfers constraining force onto executive and administrative detention power, redistributing the cost of process (time, judicial oversight, compliance) from individuals subject to state power onto the state authorities who must now justify detention judicially.
% ABSENT_VOICES: The medieval unfree classes explicitly excluded from 'liber homo' in 1215 have no voice and cannot corroborate or contest the retrofit; legal historians document their exclusion but are external commentators, not injured parties who can appear. Contemporary executive authorities object that the reading imports content the text does not bear, but their objection is treated as self-interested by beneficiaries.
% DISAPPEARANCE_RATIONALE: Courts and litigants would say the world rearranges catastrophically — due process doctrine would lose its most-cited ancestral anchor, though functionally equivalent protections exist independently in modern constitutions and human rights instruments, so the practical legal effect of removing the citation may be smaller than the rhetorical effect. Executive authorities would say little changes operationally since modern statutory and constitutional due process provisions do not depend on the 1215 text for their binding force — the citation is symbolic scaffolding on structures that now stand on their own.
% FOUNDING_PROBLEM: The 1215 charter was built to resolve a specific political crisis: rebellious barons sought to bind King John's exercise of feudal and fiscal power over their own class after years of arbitrary taxation, disseisin, and extrajudicial punishment of free tenants.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the beneficiary set (constitutional courts, litigants) corroborate that the founding problem — baronial protection from royal overreach — was resolved or superseded within a generation via subsequent reissues and the erosion of feudal tenure itself; the 'universal persons' problem this reading now addresses did not exist as the charter's founding concern and is a later constitutional and human-rights problem retrofitted onto the text.
narrative_ontology:disappearance_verdict(magna_carta_1215__universal_rights_reading, contested).
narrative_ontology:founding_problem_status(magna_carta_1215__universal_rights_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__universal_rights_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_1215__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__universal_rights_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__universal_rights_reading_tests).
:- end_tests(magna_carta_1215__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is modest (0.28) and rises slowly over the interval, reflecting the doctrine's genuine and growing coordination function (a shared citable anchor for due process across jurisdictions) alongside a real but bounded cost imposed on executive detention power. Suppression is low (0.22) because the reading operates through persuasion, precedent, and litigation rather than coercive exclusion of alternatives — courts that reject the universal reading are not silenced, they simply lose citation battles. Theater ratio is moderate and rising (0.4) because a substantial share of invocation is rhetorical/legitimating (citing 1215 for gravitas) rather than doing independent legal work beyond what modern constitutional text already provides — the founding-problem mismatch (dead original problem, contested current invocation) is exactly what theater_ratio is tracking here. Accessibility collapse is moderate (0.35): once a court accepts the universal reading as authoritative lineage, alternative textual histories become harder to argue, but the reading has never fully foreclosed the historicist counter-argument, which persists robustly in legal scholarship. Resistance is comparatively high (0.55) because legal historians and originalist jurists actively contest the anachronistic retrofit.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary/litigant seat, the universal reading is a rope: it solves a genuine coordination problem (a shared ancestral anchor for due process claims across common-law jurisdictions) with real net benefit to those it protects. From the executive-authority seat, the same doctrine functions closer to a tangled rope or even an imposed constraint: the historical claim is contested, the scope expansion beyond the 1215 text's own terms is not something they agreed to, and the enforcement (judicial review, habeas litigation) is real and costly. The engine computes these divergent per-seat readings from the declared power/exit structure; this story does not force convergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Detained persons and criminal defendants sit near the beneficiary end: the universal reading, when successfully invoked, subsidizes their position against state power by supplying constraining precedent they did not have to construct themselves. Executive detention authorities and summary punishment regimes sit near the target end: the doctrine's expansive scope directly increases their compliance burden and forecloses shortcuts they would otherwise take. Constitutional courts and litigants are agenda-setters who actively construct and maintain the reading — they are not passive beneficiaries but architects of the constraint's continued force.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is 'dead' — the actual 1215 crisis (baronial protection from royal fiscal overreach) was resolved within a generation and the specific mechanism (a charter binding one king) has no modern operative force. Yet the disappearance_verdict is 'contested' rather than 'world_unchanged,' because modern due process doctrine has independent constitutional and statutory grounding that does not depend on the 1215 citation — removing Magna Carta's rhetorical anchor would not collapse the substantive protections, only the genealogical narrative used to legitimate them. This is the mandatrophy signature: the mandate (protecting barons from King John) is dead, but the constraint (citing Clause 39 as universal precedent) persists because it now performs a different, still-live function (legitimating rhetoric for a doctrine that stands on other legs). Classifying this as pure extraction would miss the real coordination value of a shared citable anchor; classifying it as pure coordination would miss that the anchor's claimed scope exceeds what the founding document actually established.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liber_homo_original_scope_ambiguity,
    'Did ''liber homo'' in the 1215 text denote a narrow class of free tenants (excluding villeins, serfs, and women in the legal sense of the day), or did the term''s own internal logic already gesture toward broader applicability that later interpreters correctly recovered rather than invented?',
    'Philological and historical analysis of contemporaneous usage of ''liber homo'' in other 13th-century English legal instruments, cross-referenced against the charter''s own enumerated exemptions and the social composition of parties present at Runnymede.',
    'If the term was narrowly bounded by design, the universal reading is a retrospective expansion that borrows legitimacy without textual warrant — closer to a constructed doctrine using natural-law-style framing. If the term''s logic was already latent-universal, the modern reading is a legitimate unfolding rather than an invention. This is the single largest determinant of whether this reading is descriptively honest about its own genealogy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liber_homo_original_scope_ambiguity, empirical, 'Whether ''free men'' was narrowly or latently broadly scoped in the original 1215 text.').

omega_variable(
    kernel_reading_selection_pressure,
    'Is the selection of the universal-rights reading over the baronial-privilege reading driven by which reading better serves contemporary due-process litigants and courts (a beneficiary-driven selection), or by genuine historical-interpretive argument independent of who benefits?',
    'Trace the citation history: when did courts and scholars first advance the universal reading, and did that advancement track independent textual scholarship or track the emergence of parties (civil rights litigants, constitutional courts) who had structural interest in a broader precedent?',
    'If selection tracks beneficiary interest rather than independent scholarship, this reading functions partly as a false-summit dynamic — a naturalized claim (transhistorical rights) serving identifiable modern beneficiaries. If selection tracks independent scholarship, the reading has stronger claim to descriptive accuracy rather than motivated reasoning.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether the universal reading''s ascendance is beneficiary-driven or scholarship-driven.').

omega_variable(
    coke_retrofit_provenance,
    'How much of the universal reading traces to Sir Edward Coke''s 17th-century reinterpretation of Magna Carta against Stuart absolutism, versus tracing to the 1215 text''s own terms — and does that provenance matter for the reading''s legitimacy?',
    'Historical and textual analysis of Coke''s Institutes and comparison against the 1215 and 1225 charter texts to isolate exactly what Coke added versus what he found.',
    'If the universal reading is substantially Coke''s 17th-century political invention rather than a recovery of 1215 meaning, the ''transhistorical precedent'' framing may itself be a rhetorical device whose power derives from misattributed antiquity — relevant to the founding_problem status finding of ''dead.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coke_retrofit_provenance, empirical, 'The extent to which the universal reading originates with Coke rather than the 1215 charter itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__universal_rights_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__universal_rights_reading, theater_ratio, 1215, 0.05).
narrative_ontology:measurement(magn_tr_t1628, magna_carta_1215__universal_rights_reading, theater_ratio, 1628, 0.3).
narrative_ontology:measurement(magn_tr_t1791, magna_carta_1215__universal_rights_reading, theater_ratio, 1791, 0.35).
narrative_ontology:measurement(magn_tr_t1948, magna_carta_1215__universal_rights_reading, theater_ratio, 1948, 0.38).
narrative_ontology:measurement(magn_tr_t2001, magna_carta_1215__universal_rights_reading, theater_ratio, 2001, 0.42).
narrative_ontology:measurement(magn_tr_t2025, magna_carta_1215__universal_rights_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__universal_rights_reading, base_extractiveness, 1215, 0.1).
narrative_ontology:measurement(magn_be_t1628, magna_carta_1215__universal_rights_reading, base_extractiveness, 1628, 0.18).
narrative_ontology:measurement(magn_be_t1791, magna_carta_1215__universal_rights_reading, base_extractiveness, 1791, 0.22).
narrative_ontology:measurement(magn_be_t1948, magna_carta_1215__universal_rights_reading, base_extractiveness, 1948, 0.25).
narrative_ontology:measurement(magn_be_t2001, magna_carta_1215__universal_rights_reading, base_extractiveness, 2001, 0.3).
narrative_ontology:measurement(magn_be_t2025, magna_carta_1215__universal_rights_reading, base_extractiveness, 2025, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(magna_carta_1215__universal_rights_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__universal_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__universal_rights_reading, 0.1).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, magna_carta_1215__baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, magna_carta_1215__living_document_reading).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, habeas_corpus_doctrine).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, due_process_clause_us_constitution).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the magna_carta_1215 kernel. baronial_privilege_reading holds the narrowest scope (feudal contracting parties only) and this reading's expansive premise directly forecloses it within a single interpretive framework — both cannot be true of the same 'liber homo' scope simultaneously. living_document_reading holds a distinct, compatible premise (interpretive accumulation legitimately supersedes original meaning regardless of what that meaning was) and this reading's evidentiary claims about scope feed and reinforce that reading's case for legitimate development, without being logically required by it — hence influences rather than forecloses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
