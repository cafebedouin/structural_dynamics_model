% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__reformist_egalitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__reformist_egalitarian_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: vedic_dharmic_corpus__reformist_egalitarian_reading
 *   human_readable: Reformist Egalitarian Reading of Vedic Authority
 *   domain: religious/political/interpretive
 *
 * SUMMARY:
 *   The reformist egalitarian reading asserts that Vedic textual meaning must
 *   conform to constitutional equality principles; caste hierarchy is
 *   presented as historical accretion rather than scriptural essence;
 *   rational critique and constitutional law supersede traditional authority
 *   in determining legitimate interpretation. This is one reading of the
 *   contested Vedic dharmic corpus kernel, competing against hereditary
 *   monopoly readings (which claim varna hierarchy is divinely ordained and
 *   textually prescribed) and bhakti devotional readings (which claim direct
 *   access to the divine bypasses caste requirements). The reformist reading
 *   has gained state institutional support through constitutional courts,
 *   education policy, and recognition of progressive scholars, while losing
 *   the organic authority of lineage transmission. The constraint operates as
 *   a tangled rope: it solves a real coordination problem (establishing a
 *   unified interpretive framework across a pluralistic society) while
 *   simultaneously extracting from orthodox institutions and identity-locked
 *   local gatekeepers. Extraction is moderate (0.45) because the reading's
 *   core claim—that rational critique and constitutional equality override
 *   tradition—itself becomes a source of authority that substitutes for
 *   hereditary legitimacy rather than eliminating extraction entirely. The
 *   measuring interval spans the period from pre-independence traditional
 *   dominance through constitutional embedding to contemporary stabilization.
 *
 * KEY AGENTS:
 *   - Dalit movements: Structured beneficiaries with growing political power; gain textual legitimacy and legal grounds for contesting exclusion
 *   - Progressive Hindu scholars: Institutional beneficiaries; occupy interpretive authority through state-recognized credentials and academic positions
 *   - Constitutional state: Agenda-setter; enforces the reading through courts, policy, and law; derives legitimacy from establishing itself as guardian of equality
 *   - Orthodox Brahmin institutions: Powerful payers; lose interpretive monopoly and state patronage; can retreat but not exit entirely
 *   - Caste monopoly gatekeepers: Moderate-power identity-locked payers; lose local authority and income; cannot exit without losing entire social position
 *   - Orthodox vedic traditionalists: Excluded; argue for dharmic continuity; treated as cover story for hierarchy maintenance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45).
domain_priors:suppression_score(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.52).
domain_priors:theater_ratio(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__reformist_egalitarian_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__reformist_egalitarian_reading, "Reformist Egalitarian Reading of Vedic Authority").
narrative_ontology:topic_domain(vedic_dharmic_corpus__reformist_egalitarian_reading, "religious/political/interpretive").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__reformist_egalitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__reformist_egalitarian_reading, 'eb815810-75d1-4f22-8afd-330f785f3a6e').
narrative_ontology:cs_kernel_codification('eb815810-75d1-4f22-8afd-330f785f3a6e', fixed_text).
narrative_ontology:cs_authority_grounding('eb815810-75d1-4f22-8afd-330f785f3a6e', extraction).
narrative_ontology:cs_interpretation_layer_present('eb815810-75d1-4f22-8afd-330f785f3a6e').
narrative_ontology:cs_reading_relation('eb815810-75d1-4f22-8afd-330f785f3a6e', vedic_dharmic_corpus__hereditary_monopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('eb815810-75d1-4f22-8afd-330f785f3a6e', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('eb815810-75d1-4f22-8afd-330f785f3a6e', foundational, constitutional_equality_supremacy).
narrative_ontology:cs_axiom_status(constitutional_equality_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('eb815810-75d1-4f22-8afd-330f785f3a6e', constitutional_equality_supremacy, deontological).
narrative_ontology:cs_axiom('eb815810-75d1-4f22-8afd-330f785f3a6e', foundational, hierarchy_as_historical_accretion).
narrative_ontology:cs_axiom_status(hierarchy_as_historical_accretion, holdable).
narrative_ontology:cs_axiom_grounding('eb815810-75d1-4f22-8afd-330f785f3a6e', hierarchy_as_historical_accretion, empirically_contingent).
narrative_ontology:cs_axiom('eb815810-75d1-4f22-8afd-330f785f3a6e', secondary, rational_critique_as_interpretive_authority).
narrative_ontology:cs_axiom_status(rational_critique_as_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('eb815810-75d1-4f22-8afd-330f785f3a6e', rational_critique_as_interpretive_authority, instrumental).
narrative_ontology:cs_reference_frame('eb815810-75d1-4f22-8afd-330f785f3a6e', rational_constitutional_equality).
narrative_ontology:cs_drift_state('eb815810-75d1-4f22-8afd-330f785f3a6e', contemporary_post_constitutional_embedding, gap(stable, substantial, true)).
narrative_ontology:cs_created_at('eb815810-75d1-4f22-8afd-330f785f3a6e', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_movements).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, progressive_hindu_scholars).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_state).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_brahmin_institutions).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, caste_monopoly_gatekeepers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain textual and constitutional legitimacy to challenge caste-based exclusion from ritual roles, temple access, and interpretive authority. The reading provides intellectual grounding for legal claims and social mobilization against hereditary restriction. Exit would mean returning to the status of excluded outsiders with no textual basis for contestation.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_movements, beneficiary,
    organized, generational, mobile, national).

% Establish modern academic and religious interpretive authority based on historical-critical scholarship, constitutional values, and rational exegesis rather than lineage. They run universities, publish widely, shape state policy on religious affairs, and occupy official positions in interfaith bodies and cultural policy.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, progressive_hindu_scholars, beneficiary,
    institutional, biographical, mobile, national).

% Establishes and enforces the interpretive framework through constitutional courts, education policy, and recognition of religious authority. The state enforces secular equality principles and recognizes rational, scholarly interpretation as legitimate while delegitimizing hereditary monopoly claims. State enforcement machinery embeds this reading into law enforcement and policy.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_state, agenda_setter,
    institutional, generational, analytical, national).

% Bear the cost of eroded interpretive monopoly, loss of exclusive ritual authority, and delegitimization of lineage-based gatekeeping. They can retreat to private practice and community institutions but face legal challenges to temple control, reduced state patronage, and loss of social authority in public discourse. Their exit would mean abandoning claim to universal Vedic interpretation.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_brahmin_institutions, payer,
    powerful, generational, constrained, national).

% Hold status and income from controlling access to ritual services, temple roles, and interpretive judgment at the local level. The reading undermines their authority by establishing that ritual knowledge is learnable, not inherited, and that constitutional equality overrides traditional gatekeeping. They cannot exit without losing their entire social position.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, caste_monopoly_gatekeepers, payer,
    moderate, biographical, identity_locked, local).

% Are structurally excluded from the interpretive conversation by the reading's foundational commitment to constitutional equality and rational critique as superior to traditional authority. They argue for dharmic continuity and the organic unfolding of scriptural meaning; this reading treats their arguments as cover for hierarchy maintenance. Their exclusion from policy-making is enforced by state apparatus and academic credentialing.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_vedic_traditionalists, excluded,
    powerful, civilizational, trapped, global).

% Adjudicate disputes between the reading and hereditary claims, interpreting fundamental rights against religious tradition. They have structured the framework so far (recognizing right to worship, striking down explicit caste restrictions, mandating non-discrimination in temple management) and could revise it if the constitutional consensus shifted.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Operate in a space this reading neither directly enables nor blocks—direct devotional access to the divine. They provide an alternative axis of spiritual authority that does not require either caste legitimation or state constitution. They are not organized as a voting block in the state interpretation struggle.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, bhakti_practitioners, observer,
    moderate, biographical, mobile, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_state).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__reformist_egalitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared framework for textual meaning: rational, historical-critical scholarship as the arbiter of what the Vedas actually say versus what traditional gatekeepers claim they say. Provides a common language (constitutional equality, scientific reasoning) for challenging hereditary monopoly and creating a unified legal-religious space where all are read as equal before both text and law.
% TRANSFER_FUNCTION: Transfers interpretive authority from hereditary Brahmin institutions to the constitutional state and modernist scholars; transfers access to ritual and spiritual roles from birth-restricted to merit-open. The constraint moves deference: from paying deference to lineage gatekeepers to paying deference to courts and credentialed scholars. Those who held ritual monopoly bear the cost of lost authority and income; those excluded gain standing to contest.
% ABSENT_VOICES: Strict hereditary traditionalists who argue the Vedic varna system is divinely ordained and that rational critique is itself a Western imposition destructive of organic dharmic unfolding. They are excluded from policy-making bodies and state-recognized interpretive authority by the reading's structural commitment to constitutional supremacy. Conservative religious communities that rely on lineage-based transmission also lose a voice in defining what legitimacy means.
% DISAPPEARANCE_RATIONALE: If this reading and its state enforcement disappeared, orthodox institutions would regain uncontested authority to define ritual roles, temple access, and interpretive legitimacy. Dalit communities would lose legal grounds for contesting exclusion. Religious authority would revert to lineage-based gatekeeping. The entire structure of secular equality in Indian religious law would collapse, and courts would no longer treat constitutional principles as superior to traditional authority claims.
% FOUNDING_PROBLEM: Caste-based exclusion from ritual and interpretive roles persisted despite modern educational access and constitutional guarantees. Traditional gatekeepers used scriptural authority as justification for excluding non-Brahmins, Dalits, and women from meaningful participation in religious life and authority structures. The problem was that hereditary restriction appeared to rest on unchangeable scriptural truth rather than human choice.
% FOUNDING_PROBLEM_CORROBORATION: Dalit intellectuals, constitutional scholars, and human rights organizations document ongoing gatekeeping and exclusion. Court cases challenging temple access, priesthood restrictions, and religious authority establish the problem as live. Constitutional courts have repeatedly affirmed the founding problem persists. Orthodox institutions themselves testify to the problem (by resisting change), confirming that exclusionary gatekeeping remains active.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__reformist_egalitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__reformist_egalitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__reformist_egalitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).
:- end_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.28 at t=0 (early independence, reading not yet institutionalized) to 0.45 at t=80 (contemporary stabilization), following the reading's increased enforcement through courts and education policy. The trajectory shows not linear accumulation but saturation: extractiveness plateaus after t=60 when the reading achieves its maximum institutional embedding. Theater ratio tracks institutional embedding: rises from 0.22 to 0.38, reflecting increasing performative endorsement by institutions (state recognition, school curricula) even as core gatekeeping persists in local practice. Suppression_requirement shows the enforcement machinery's growth: rises from 0.35 to 0.52 as courts and state actively suppress hereditary monopoly claims, require non-discrimination policies, and delegitimize lineage-based authority. The suppression is not primarily violent coercion but epistemic and institutional: delegitimization of traditionalist arguments, selective credentialing of modernist scholars, constitutional legal frameworks that treat traditionalist claims as incompatible with fundamental rights. Accessibility_collapse (0.48) reflects that the reading leaves middle ground: one can either accept rational-constitutional authority OR retreat into private traditionalism, but the public interpretive space is now closed to hereditary monopoly claims. Resistance (0.71) is high because orthodox institutions and traditionalists actively mount real opposition through counter-interpretation, private institution maintenance, and contestation of court judgments.
 *
 * PERSPECTIVAL GAP:
 *   Each seat's classification diverges because their structural relationship to the constraint is asymmetric. An orthodox institutional seat computes the constraint as snare or enforced extraction (required participation, losses, enforcement machinery). A Dalit movement seat computes it as beneficial coordination (opens access, provides legal grounds, subsidizes participation). A progressive scholar seat might compute it as rope (genuine coordination, but also consolidates their authority). The engine produces these divergent computations from the same structural data because directionality is position-specific: the same constraint that extracts from one seat subsidizes another. This is the intended measurement—per-seat classification divergence is not error, it is the apparatus's most important output.
 *
 * DIRECTIONALITY LOGIC:
 *   Who benefits: Dalit movements and excluded communities gain access to scriptural legitimacy and legal grounds for contestation (low d, net subsidized). Progressive scholars gain occupancy of public interpretive authority and state recognition (low-moderate d, net beneficiaries). Constitutional state gains legitimacy and control of a key institutional domain (moderate d, mixed—both enforces and benefits). Who pays: Orthodox Brahmin institutions lose interpretive monopoly, state patronage, and cultural authority (high d, net victims). Caste monopoly gatekeepers lose local authority and income; they are identity_locked because their entire social position rests on gatekeeping (very high d=0.8+, trapped victims). Exit options determine directionality amplification: gatekeepers have no exit (identity_locked), so their d approaches 1.0 (full extraction target); dalit movements have mobile options (can work through other channels) but the reading opens the most valuable one (constitutional legitimacy), so their d sinks (full beneficiary). Orthodox institutions are powerful and can maintain private practice but face legal constraint (constrained exit, d moderate-high). Traditionalists are analytically excluded, so directionality does not apply to them—they are out of the game by structural design.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy would occur if (1) the founding problem were dead (caste-based exclusion fully resolved) AND (2) the constraint persisted with zero coordination function (pure theater and inertia). Neither is present. The founding problem is live: Dalit exclusion from temple roles, priesthood restrictions, and gatekeeping persist. The coordination function is active: the reading continues to provide the only framework in which these exclusions can be legally contested. Theater_ratio rising indicates increasing gap between formal equality and actual gatekeeping, but this is institutional normal lag, not mandatrophy—a constraint can have high theater and still be doing its job if the theater masks persistent substantive work. Mandatrophy would show as theater_ratio approaching 1.0 while founding_problem_status shifted to 'dead' and the constraint persisted anyway. That has not occurred.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_essentialism_vs_accretion,
    'Is caste hierarchy an essential element of Vedic dharma (as hereditary readings claim) or a historical accretion imposed on the texts (as the reformist reading claims)?',
    'Comparative textual analysis, archaeological and historical scholarship on the chronology of caste concepts in the Vedic corpus, study of early Vedic social structures versus later Brahminic systematization.',
    'If hierarchy is essential and textually foundational, the reformist reading''s core empirical claim fails and its interpretive framework becomes pure value assertion disconnected from textual grounding. If accretion is established, the reading gains empirical support for rational reinterpretation. Either way, the constitutional commitment (equality overrides tradition) can persist, but its textual legitimacy shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_essentialism_vs_accretion, empirical, 'Whether caste hierarchy is essential to or accretive upon the Vedic corpus.').

omega_variable(
    constitutional_supremacy_contestability,
    'Is the reformist reading''s foundational axiom—that constitutional equality principles supersede traditional religious authority—itself contestable, or has it achieved consensus status in Indian legal culture?',
    'Tracking constitutional court rulings, legislative challenges to secular equality framing, emergence of theocratic political movements that contest constitutional supremacy, shifts in majority opinion on whether religious tradition should override secular law.',
    'If the constitutional commitment achieves deep consensus, the reading becomes institutionally stable and threat-resistant. If it remains contested at the constitutional level itself, the reading''s legitimacy could be revoked by a future constitutional moment. A foreseeable drift would be the ''overridden'' status of the constitutional_supremacy axiom if a successful constitutional amendment or majoritarian reframing were to establish religious tradition as a competing foundational principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_supremacy_contestability, preference, 'Whether the reading''s constitutional grounding is stable or contingent on political consensus.').

omega_variable(
    rational_critique_as_universal_vs_cultural_imposition,
    'Is rational-critical scholarship a universal epistemic standard, or is it a specific cultural tradition (Western, secular, modern) being imposed on Hindu religious interpretation?',
    'Philosophical analysis of rationality claims, investigation of whether non-Western epistemologies (lineage-based knowledge transmission, intuitive insight, revelation) can coexist with rationalism as valid interpretive methods, documentation of how power asymmetries shape which methods are credentialed.',
    'If rationalism is universal, the reading''s epistemic framework is justified. If it is culturally specific, the reading becomes an imposition of Western epistemology on Hindu texts, and the constraint shifts from emancipatory coordination to epistemic colonization. This would require reclassification: the extraction would be epistemic rather than material (loss of interpretive authority to a foreign framework), and the beneficiaries (progressive scholars, constitutional state) would be complicit in imposing Western rationality. The reading''s tangled_rope classification could degrade to snare if the rational_critique axiom is reframed as imposed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rational_critique_as_universal_vs_cultural_imposition, conceptual, 'Whether rational critique is a universal epistemic standard or a culturally specific imposition.').

omega_variable(
    identity_lock_reversibility,
    'Can caste monopoly gatekeepers exit their identity_locked position if the reading''s enforcement were removed, or is their identity fusion irreversible?',
    'Study of communities where the reading has been locally suspended or reversed (certain orthodox communities, diaspora traditionalist movements), tracking whether gatekeepers regain social position and whether they retain internalized egalitarian values even after external enforcement ceases.',
    'If the lock is reversible (gatekeepers can return to monopoly gatekeeping if enforcement ends), their actual exit options are more constrained than identity_locked suggests, pushing d higher. If irreversible (younger generations raised under the reading cannot unsee rational critiques of hierarchy even if institutional enforcement disappears), the reading has achieved internalization and the suppression is partly epistemic. This affects the theater_ratio interpretation: if internalization is deep, lower theater_ratio might indicate genuine value shift rather than performance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether identity lock to caste gatekeeping can be reversed if enforcement ends.').

omega_variable(
    kernel_reading_committer_distinction,
    'Is this constraint a reading-specific account (the reformist reading''s own framing), or an observer-level classification of what the reading does structurally?',
    'The authored beneficiaries (dalit_movements, progressive_scholars, constitutional_state) are those who gain from this reading''s institutional embedding. The authored victims (orthodox institutions, caste gatekeepers) are those who lose. The committer frame (what the reformist reading claims about itself and its relationship to the kernel) versus the observer frame (structural facts about who benefits, who pays) are distinct. The story is authored as observer-level structural data, not as reformist self-assertion.',
    'If the story is misread as the reformist reading''s self-assertion, it becomes propaganda. If read as structural analysis of what the reading does, it is descriptive. The omegas and commentary clarify the distinction by naming the reading''s contestability (rational_critique_universality, textual_essentialism) and showing that the reading''s own grounding is subject to omega uncertainty, not sacred.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_distinction, conceptual, 'Clarification that this is structural analysis of a reading, not the reading''s own self-description.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__reformist_egalitarian_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(vedi_tr_t0, observed).
narrative_ontology:measurement(vedi_tr_t10, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(vedi_tr_t10, observed).
narrative_ontology:measurement(vedi_tr_t20, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(vedi_tr_t20, observed).
narrative_ontology:measurement(vedi_tr_t40, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement_basis(vedi_tr_t40, observed).
narrative_ontology:measurement(vedi_tr_t60, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement_basis(vedi_tr_t60, observed).
narrative_ontology:measurement(vedi_tr_t80, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement_basis(vedi_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(vedi_be_t0, observed).
narrative_ontology:measurement(vedi_be_t10, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement_basis(vedi_be_t10, observed).
narrative_ontology:measurement(vedi_be_t20, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(vedi_be_t20, observed).
narrative_ontology:measurement(vedi_be_t40, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement_basis(vedi_be_t40, observed).
narrative_ontology:measurement(vedi_be_t60, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 60, 0.45).
narrative_ontology:measurement_basis(vedi_be_t60, observed).
narrative_ontology:measurement(vedi_be_t80, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 80, 0.45).
narrative_ontology:measurement_basis(vedi_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(vedi_su_t0, observed).
narrative_ontology:measurement(vedi_su_t10, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(vedi_su_t10, observed).
narrative_ontology:measurement(vedi_su_t20, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement_basis(vedi_su_t20, observed).
narrative_ontology:measurement(vedi_su_t40, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 40, 0.51).
narrative_ontology:measurement_basis(vedi_su_t40, observed).
narrative_ontology:measurement(vedi_su_t60, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement_basis(vedi_su_t60, observed).
narrative_ontology:measurement(vedi_su_t80, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 80, 0.52).
narrative_ontology:measurement_basis(vedi_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__reformist_egalitarian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.1).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus__hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus__bhakti_devotional_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, indian_constitutional_secularism__religious_equality).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, brahminical_ritual_monopoly__local_gatekeeping).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the vedic_dharmic_corpus kernel. The constraint family includes three readings: hereditary_monopoly_reading (varna hierarchy divinely ordained), bhakti_devotional_reading (devotion bypasses caste), and this reformist_egalitarian_reading (rational critique and constitutional equality override tradition). Each reading has a different epsilon, beneficiary structure, and type. The three readings coexist in contemporary Indian religious and political life without logical resolution—they are held by different institutional constituencies. Network edges trace influence: the reformist reading gains state institutional embedding, which structurally pressures (influences, does not foreclose) the hereditary reading by shifting legal standing and social authority. The bhakti reading occupies a parallel non-institutional space and coexists with both. All three are linked through the single kernel (the Vedic corpus and its interpretive authority).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_dharmic_corpus__reformist_egalitarian_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
