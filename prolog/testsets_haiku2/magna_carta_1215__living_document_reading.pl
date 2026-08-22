% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__living_document_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__living_document_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: magna_carta_1215__living_document_reading
 *   human_readable: Magna Carta as Living Constitutional Document
 *   domain: constitutional_law/legal_history
 *
 * SUMMARY:
 *   Magna Carta, originally a feudal contract negotiated between English
 *   barons and King John in 1215, has been reinterpreted through eight
 *   centuries of precedent as a foundational constitutional constraint on
 *   arbitrary governmental action. The living-document reading treats this
 *   reinterpretation as legitimate constitutional development: Clause 39 ('no
 *   free man shall be imprisoned or seized except by lawful judgment') is
 *   read as emitting a continuous duty on government to justify detention and
 *   coercion through procedurally fair means, expanded and adapted across
 *   generations to address arbitrary police power, administrative overreach,
 *   and modern surveillance. The original baronial meaning (protection of
 *   landed property and feudal incidents) has been structurally superseded by
 *   a universal due-process principle. The reading coexists with two
 *   alternatives: the baronial-privilege reading (Magna Carta addresses only
 *   feudal contracts, and modern extension is rewriting, not development) and
 *   the universal-rights reading (Magna Carta carried an implicit
 *   transhistorical rights principle that was always there, merely obscured
 *   by medieval language). This story instantiates only the living-document
 *   reading—ε-invariantly—with authority grounded in lineage (the chain of
 *   judicial transmission) and interpretation layered at every stage. The
 *   claim/metric gap is deliberate: the reading claims rope (genuine
 *   coordination function enabling constitutional adaptation), and the
 *   metrics describe moderate extractiveness (interpretive authority
 *   concentrates in the judiciary, constrained choice for originalists) with
 *   low suppression (contestation is loud and ongoing, not silenced).
 *
 * KEY AGENTS:
 *   - Interpretive judiciary: sets the canonical meaning of Clause 39 through precedent; benefits from flexibility to reinterpret.
 *   - Constitutional scholars: defend and develop the living-document framework; occupy the center of legal discourse.
 *   - Originalist jurists: challenge the reading's premise that precedent can redefine the constraint; excluded from dominant authority.
 *   - Administrative branch: operates under constraints reinterpreted far beyond the original text.
 *   - Common-law jurisdictions: sustain the precedential authority structure across centuries and geographies.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, 0.31).
domain_priors:suppression_score(magna_carta_1215__living_document_reading, 0.18).
domain_priors:theater_ratio(magna_carta_1215__living_document_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__living_document_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__living_document_reading, "Magna Carta as Living Constitutional Document").
narrative_ontology:topic_domain(magna_carta_1215__living_document_reading, "constitutional_law/legal_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__living_document_reading, '438ded82-47c3-44ed-b26b-d4873e89aef0').
narrative_ontology:cs_kernel_codification('438ded82-47c3-44ed-b26b-d4873e89aef0', fixed_text).
narrative_ontology:cs_authority_grounding('438ded82-47c3-44ed-b26b-d4873e89aef0', lineage).
narrative_ontology:cs_interpretation_layer_present('438ded82-47c3-44ed-b26b-d4873e89aef0').
narrative_ontology:cs_reading_relation('438ded82-47c3-44ed-b26b-d4873e89aef0', magna_carta_1215__baronial_privilege_reading, coexists_with).
narrative_ontology:cs_reading_relation('438ded82-47c3-44ed-b26b-d4873e89aef0', magna_carta_1215__universal_rights_reading, coexists_with).
narrative_ontology:cs_axiom('438ded82-47c3-44ed-b26b-d4873e89aef0', foundational, precedential_accumulation_constitutes_development).
narrative_ontology:cs_axiom_status(precedential_accumulation_constitutes_development, holdable).
narrative_ontology:cs_axiom_grounding('438ded82-47c3-44ed-b26b-d4873e89aef0', precedential_accumulation_constitutes_development, conventional).
narrative_ontology:cs_axiom('438ded82-47c3-44ed-b26b-d4873e89aef0', secondary, interpretive_lineage_confers_legitimacy).
narrative_ontology:cs_axiom_status(interpretive_lineage_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('438ded82-47c3-44ed-b26b-d4873e89aef0', interpretive_lineage_confers_legitimacy, conventional).
narrative_ontology:cs_reference_frame('438ded82-47c3-44ed-b26b-d4873e89aef0', continuous_precedential_reinterpretation_framework).
narrative_ontology:cs_drift_state('438ded82-47c3-44ed-b26b-d4873e89aef0', contemporary_administrative_state_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('438ded82-47c3-44ed-b26b-d4873e89aef0', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__living_document_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, interpretive_judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, constitutional_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, administrative_branch).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Magna Carta through accumulated precedent rather than original baronial context. Each generation of judges extends, reframes, and applies Clause 39 to new domains: trial by jury, due process, equal protection. The judiciary's authority to reinterpret depends on the premise that Magna Carta grows through precedential accretion, not on locked original meaning. Judges benefit from interpretive flexibility: they can legitimately address new harms (arbitrary imprisonment, administrative overreach) without formal amendment.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, interpretive_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Scholarly tradition that treats Magna Carta as a living framework. Academic interpretation, law review articles, and historical scholarship emphasizing evolutionary development secure professional standing and intellectual authority. The living-document framing is the dominant reading in elite legal education; scholars defending this reading occupy the center of constitutional discourse.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, constitutional_scholars, beneficiary,
    organized, biographical, mobile, national).

% Argue that Magna Carta's meaning was fixed at ratification (1215 or the reissues of 1217, 1225); subsequent reinterpretation, they claim, rewrites the constraint rather than developing it. They are not in the primary decision-making seat — the living-document reading dominates appellate authority in most common-law jurisdictions — but they contest the premise that precedent can legitimate reinterpretation beyond the original scope. If present, they would argue the constraint has been colonized by modern values.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, originalist_jurists, excluded,
    powerful, biographical, constrained, national).

% The class for whom Magna Carta was originally negotiated in 1215 — landowning magnates seeking protection from royal arbitrary seizure of property and feudal incidents. By the living-document reading, their specific historical demands have been subordinated to a universal constitutional principle that no longer addresses their interests. They are excluded from the modern debate not by design but by historical distance; their seat is acknowledged only through historical scholarship.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, historical_barons_and_successors, excluded,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_non_agent(magna_carta_1215__living_document_reading, historical_barons_and_successors).

% Executive and administrative agencies operate under Clause 39 constraints as reinterpreted by living-document reasoning. Modern due-process doctrine (notice, hearing, reasoned decision-making) constrains administrative action far beyond what the medieval barons' negotiation touched. The executive must justify detention, searches, and regulatory takings through reasoning the judiciary accepts as evolved Magna Carta doctrine, not through the original 1215 framing.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, administrative_branch, payer,
    institutional, generational, constrained, national).

% Britain, Canada, Australia, and the Commonwealth sustain the living-document reading as the dominant interpretive authority. Judicial decisions in each jurisdiction contribute to the precedential tradition; the constraint is enforced through case law accumulation and judicial review of executive and legislative action.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, common_law_jurisdictions, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a durable, adaptive constitutional framework for due-process rights that can be extended to new contexts (modern police power, administrative rulemaking, digital surveillance) without formal amendment. The precedential mechanism allows constitutionalism to grow across centuries without the institutional friction of rewriting the foundational text.
% TRANSFER_FUNCTION: Transfers interpretive authority from original textual intent to accumulated judicial precedent. The judiciary gains the power to declare what Magna Carta demands in each generation; originalist challenges to that authority are structurally subordinated; modern citizens inherit expanded protections the 1215 signatories did not contemplate, at the cost of accepting that their constitutional rights are defined by judges, not by fixed text.
% ABSENT_VOICES: Originalist constitutionalists argue the living-document reading has colonized Magna Carta's meaning and removed the possibility of fidelity to the fixed constraint. Historical scholars focused on the baronial context are typically unheard in the appellate courts where the constraint's modern meaning is set. Competing constitutional traditions (written-constitution systems, civil-law jurisdictions) do not participate in common-law precedential authority.
% DISAPPEARANCE_RATIONALE: If the living-document reading vanished and the judiciary reverted to strict originalism, constitutional protections would narrow sharply: administrative due process, implied privacy rights, and jury-trial extensions would lose their doctrinal footing. Executive action would face fewer judicially-enforced limits. The constitutional landscape would fragment into disputes over original meaning with no agreed mechanism for settling new questions.
% FOUNDING_PROBLEM: Medieval Magna Carta addressed arbitrary royal seizure, feudal exactions, and denial of trial. As governance evolved (parliamentary authority, administrative agencies, police power, electronic surveillance), new forms of arbitrary action emerged that the original text did not contemplate. The living-document reading solves the problem of how to extend constitutional constraint to new harms without constant formal amendment.
% FOUNDING_PROBLEM_CORROBORATION: Appellate courts in common-law jurisdictions, legal scholarship, and constitutional democracy practitioners outside the judiciary attest that new forms of arbitrary governmental action continuously arise and that the precedential mechanism allows continuous extension of constitutional constraint. Originalists attest that the problem is over-solved: the reading has drifted so far from 1215 text that it no longer constrains interpretation at all, but this attestation concedes the technical problem (how to adapt) is live—they dispute the solution's legitimacy, not the problem's existence.
narrative_ontology:disappearance_verdict(magna_carta_1215__living_document_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__living_document_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__living_document_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_1215__living_document_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__living_document_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__living_document_reading_tests).
:- end_tests(magna_carta_1215__living_document_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.31) because the constraint genuinely solves a coordination problem (how to adapt constitutional constraint across centuries without formal amendment) and the beneficiaries (judiciary, scholars) do not suppress alternatives—they argue openly for their reading. The measurement series shows slow accumulation: extractiveness and theater both rise over the 811-year interval as the gap between original meaning and modern application widens. By 2026, extractiveness is higher (0.31) because the reinterpretation has departed further from the original text; theater rises correspondingly (0.22) because more of the constraint's operation is spent justifying reinterpretation to originalist challengers, not merely applying the rule. Suppression is stable and low (0.18) because originalist challenges are not effectively silenced—they appear in appellate dissents, law review articles, and legislative proposals; the living-document reading dominates but does not exclude the competing reading from discourse. The temporal trend models the constraint's evolution: it began as a specific feudal contract (low extractiveness, no gap between text and application, low theater) and has become a meta-constraint on how constitutional meaning can be generated (higher extractiveness because the judiciary's interpretive authority is more valuable as the gap widens, rising theater because more enforcement energy goes to defending the legitimacy of reinterpretation itself).
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, the constraint is legitimate coordination: it enables constitutional governance to adapt without breaking the foundational text. From an originalist seat, the same structure is interpretive capture: the judiciary has colonized Magna Carta's meaning and removed the possibility of textual fidelity. From the common-law jurisdictions' analytical seat, the constraint is a meta-framework that scaffolds contestation—both readings remain defensible, and the authority structure (appellate precedent) does not resolve the dispute but channels it through case-law accumulation. The engine will compute these divergences from the structural data: the judiciary's low directionality (beneficiary) will yield low effective extraction, while the originalist jurist's constrained exit and excluded role will yield higher computed extraction despite their formal power. This divergence is the measurement—the reading's legitimacy claim (rope) sits alongside metrics that show moderate extraction and ongoing suppression of originalist authority, and that gap is where the court discovers whether the reading is coordination or capture.
 *
 * DIRECTIONALITY LOGIC:
 *   The interpretive judiciary occupies the beneficiary seat: it holds the power to declare what Magna Carta requires, and its authority is sustained by the living-document premise that precedent legitimizes reinterpretation. Its d is low (~0.2), reflecting beneficiary status—the constraint enables its power. Constitutional scholars benefit from the dominant reading (academic standing flows from defending evolutionary constitutionalism) but have lower power and less direct enforcement authority; they coexist as organized beneficiaries (d ~0.3). The administrative branch pays through constraint: it must justify action under expanded due-process doctrine that goes far beyond 1215 meaning (d ~0.7). Originalist jurists are not payers in the sense of bearing direct cost; they are excluded from the dominant authority structure and face diminishing persuasiveness in courts, but they retain intellectual standing and can mount legislative challenges (d ~0.4, a constrained actor with modest exit). Common-law jurisdictions are observers: they sustain the authority structure but are not themselves positioned as targets or beneficiaries—the constraint operates through their courts but does not extract from or benefit them as institutional actors (d ~0.5, analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   The living-document reading avoids mandatrophy by maintaining a genuine coordination function: it solves the structural problem of how a 1215 text can govern 21st-century governance without constant amendment. The founding problem (how to extend constitutional constraint to new harms) is live, and the reading's mechanism (precedential reinterpretation) is still deployed. However, a secondary mandatrophy question arises: as the gap between original meaning and modern application widens, does the constraint's legitimacy depend increasingly on theater (defending the reading's premise) rather than on its actual coordination work? The theater_ratio measurement (0.22 by 2026) suggests that roughly one-fifth of the constraint's enforcement energy goes to justifying reinterpretation, not to applying the rule. If theater_ratio crosses 0.5, the constraint would be operating more as performance than as function, and mandatrophy would threaten.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    axiom_contestation_irresolubility,
    'Is precedential reinterpretation of Magna Carta''s meaning a legitimate development of the constraint''s scope, or a rewriting that breaks fidelity to the foundational text?',
    'No empirical data resolves this: it is a conceptual dispute about what counts as ''development'' versus ''change.'' Originalist and living-document jurisprudence cite the same case law and historical record but read them as supporting opposite conclusions. The question is located in the axiom: does precedential accumulation constitute legitimate constitutional development (living-document axiom) or does it constitute rewriting (originalist axiom)? Both cannot be true in any single judicial framework, yet both remain live positions defended by competent jurists.',
    'If precedential accumulation constitutes legitimate development, the living-document reading is a rope: the authority structure genuinely solves the coordination problem of constitutional adaptation. If rewriting, the reading is a snare: the judiciary extracts interpretive authority by claiming precedential legitimacy, and the reading obscures that extraction through the framing of development. The computed type depends on which axiom the jurisdiction endorses as foundational.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(axiom_contestation_irresolubility, conceptual, 'Whether precedential accumulation is legitimate development or illegitimate rewriting.').

omega_variable(
    authority_lineage_exhaustion,
    'As the gap between original baronial meaning and modern constitutional principle widens, does the lineage of judicial interpretation continue to confer legitimacy, or does the gap become so wide that the lineage itself requires rejustification?',
    'Historical analysis of judicial reasoning: does appellate authority argue that modern doctrine flows from the precedential chain, or does it increasingly argue for direct normative principle (rights, equality, dignity) and cite precedent only as support? If reasoning shifts from lineage-based to principle-based, the authority structure may be reconstituting itself, not merely extending an inherited line.',
    'If lineage exhausts (the chain of reinterpretation breaks or becomes implausible), the constraint''s authority would depend on something other than precedent—either direct normative principle or new founding authorization. The living-document reading would no longer be sustainable; one of the alternative readings might become dominant, or a new reading might emerge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_lineage_exhaustion, empirical, 'Whether the judicial lineage can sustain itself as the gap between original and modern meaning widens indefinitely.').

omega_variable(
    reading_coexistence_mechanism,
    'How does the common-law authority structure maintain legitimacy for the living-document reading while the originalist alternative is excluded from appellate dominance?',
    'Institutional analysis of how competing readings interact: do originalist opinions appear as forceful dissents, get cited in lower courts, appear in legislation and constitutional amendment proposals? If so, coexistence is maintained through institutional pluralism (multiple seats, multiple authority channels). If originalist positions are silenced or confined to academia, the mechanism breaks and suppression rises—the reading becomes more snare-like.',
    'If coexistence is maintained through pluralism, suppression stays low (0.18) and the reading remains a rope. If originalist positions are effectively silenced, suppression would rise and the reading would compute as tangled_rope or snare—extraction hidden beneath a coordination story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_coexistence_mechanism, empirical, 'The institutional mechanism sustaining coexistence of the living-document and originalist readings.').

omega_variable(
    precedent_legitimacy_premise,
    'What makes precedent binding? Is it deference to institutional authority (the judiciary''s power to declare meaning), respect for stability (reliance on settled doctrine), fidelity to the text (precedent found in the text, not invented), or some combination?',
    'Jurisprudential analysis: do judicial opinions ground precedential authority in respect for prior decisions, in the continuity of reasoning, in the text, or in deference to institutional role? Different groundings make different claims about what counts as legitimate reinterpretation.',
    'If precedent is legitimate because of institutional authority and stability, the living-document reading is sustainable regardless of the gap from original meaning—the constraint grows because the judiciary''s power to declare meaning grows. If precedent is legitimate only because it is found in the text, a wide gap between text and doctrine would delegitimize precedent, and the reading would become a snare (extraction masked by false legitimacy claim). The computed extractiveness depends partly on this axiom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_legitimacy_premise, conceptual, 'The foundational justification for precedential authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__living_document_reading, 1215, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__living_document_reading, theater_ratio, 1215, 0.08).
narrative_ontology:measurement(magn_tr_t1400, magna_carta_1215__living_document_reading, theater_ratio, 1400, 0.1).
narrative_ontology:measurement(magn_tr_t1688, magna_carta_1215__living_document_reading, theater_ratio, 1688, 0.13).
narrative_ontology:measurement(magn_tr_t1832, magna_carta_1215__living_document_reading, theater_ratio, 1832, 0.16).
narrative_ontology:measurement(magn_tr_t1950, magna_carta_1215__living_document_reading, theater_ratio, 1950, 0.19).
narrative_ontology:measurement(magn_tr_t2026, magna_carta_1215__living_document_reading, theater_ratio, 2026, 0.22).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__living_document_reading, base_extractiveness, 1215, 0.05).
narrative_ontology:measurement(magn_be_t1400, magna_carta_1215__living_document_reading, base_extractiveness, 1400, 0.08).
narrative_ontology:measurement(magn_be_t1688, magna_carta_1215__living_document_reading, base_extractiveness, 1688, 0.12).
narrative_ontology:measurement(magn_be_t1832, magna_carta_1215__living_document_reading, base_extractiveness, 1832, 0.18).
narrative_ontology:measurement(magn_be_t1950, magna_carta_1215__living_document_reading, base_extractiveness, 1950, 0.24).
narrative_ontology:measurement(magn_be_t2026, magna_carta_1215__living_document_reading, base_extractiveness, 2026, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__living_document_reading, suppression_requirement, 1215, 0.1).
narrative_ontology:measurement(magn_su_t1400, magna_carta_1215__living_document_reading, suppression_requirement, 1400, 0.12).
narrative_ontology:measurement(magn_su_t1688, magna_carta_1215__living_document_reading, suppression_requirement, 1688, 0.14).
narrative_ontology:measurement(magn_su_t1832, magna_carta_1215__living_document_reading, suppression_requirement, 1832, 0.16).
narrative_ontology:measurement(magn_su_t1950, magna_carta_1215__living_document_reading, suppression_requirement, 1950, 0.17).
narrative_ontology:measurement(magn_su_t2026, magna_carta_1215__living_document_reading, suppression_requirement, 2026, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__living_document_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__living_document_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__universal_rights_reading).

% DUAL FORMULATION NOTE:
% The Magna Carta 1215 kernel decomposes into three constraint stories, each instantiating a different reading: baronial_privilege_reading (ε ≈ 0.05, mountain: the constraint is the feudal contract itself, no reinterpretation); universal_rights_reading (ε ≈ 0.18, rope: a transhistorical rights principle that reinterpretation discovers but does not create); living_document_reading (this one, ε ≈ 0.31, rope: reinterpretation is legitimate development that creates constitutional meaning through precedent). The readings coexist across different authority seats: common-law appellate courts predominantly adopt the living-document reading; originalist jurists defend the baronial/fixed-meaning reading; constitutional scholars debate the universal-rights principle. All three readings are live options in contemporary jurisprudence. The stories are linked through network.affects_constraints and document the kernel contestation. No reading forecloses another—the structure is three coexisting positions, not a resolved unification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
