% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__exogenous_override_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_commitment_legitimacy__exogenous_override_reading
 *   human_readable: Federal Coercion of Doctrinal Practice Suspension (Exogenous Override Reading)
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   In 1890, facing federal coercion—territorial exclusion, property seizure,
 *   legal persecution—the LDS Church's First Presidency issued the Manifesto,
 *   publicly renouncing the practice of plural marriage. This reading
 *   interprets the Manifesto as extraction under duress: federal government
 *   benefits from demonstrating that no religious institution can resist
 *   federal supremacy; LDS membership and hierarchy bear the cost of publicly
 *   abandoning a core theological commitment. The reading claims the
 *   underlying doctrine (eternal marriage, celestial polygamy as divine
 *   principle) remains unchanged in LDS theology—only practice is suspended
 *   under external force. This creates a permanent legitimacy crisis for
 *   members: if the doctrine is true, why was it abandoned? If it was false,
 *   how was it ever revealed? The constraint's persistence depends on the
 *   hierarchy's continuing public compliance and suppression of any
 *   institutional narrative that treats doctrine and practice as genuinely
 *   separated.
 *
 * KEY AGENTS:
 *   - federal_government: institutional beneficiary, exercises coercive force (territorial exclusion, property seizure, legal sanctions)
 *   - lds_ecclesiastical_hierarchy: powerful but trapped, extracts doctrinal practice suspension under threat of institutional annihilation
 *   - lds_membership: organized but identity-locked, bears legitimacy crisis and theological uncertainty
 *   - federal_legislative_authority: institutional beneficiary, vindicated supremacy doctrine
 *   - anti_polygamy_political_coalition: organized beneficiary, collects political victory
 *   - polygamist_practitioners: excluded and powerless, structurally silenced in hierarchy's capitulation
 *   - women_in_polygamist_families: doubly excluded (neither LDS nor federal representation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, 0.82).
domain_priors:suppression_score(marriage_commitment_legitimacy__exogenous_override_reading, 0.76).
domain_priors:theater_ratio(marriage_commitment_legitimacy__exogenous_override_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_legitimacy__exogenous_override_reading, "Federal Coercion of Doctrinal Practice Suspension (Exogenous Override Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__exogenous_override_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__exogenous_override_reading, '70596455-c449-4421-9daa-b5edd29486de').
narrative_ontology:cs_kernel_codification('70596455-c449-4421-9daa-b5edd29486de', formalized).
narrative_ontology:cs_authority_grounding('70596455-c449-4421-9daa-b5edd29486de', extraction).
narrative_ontology:cs_interpretation_layer_present('70596455-c449-4421-9daa-b5edd29486de').
narrative_ontology:cs_reading_relation('70596455-c449-4421-9daa-b5edd29486de', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('70596455-c449-4421-9daa-b5edd29486de', marriage_commitment_legitimacy__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('70596455-c449-4421-9daa-b5edd29486de', foundational, federal_supremacy_overrides_religious_doctrine).
narrative_ontology:cs_axiom_status(federal_supremacy_overrides_religious_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('70596455-c449-4421-9daa-b5edd29486de', federal_supremacy_overrides_religious_doctrine, empirically_contingent).
narrative_ontology:cs_axiom('70596455-c449-4421-9daa-b5edd29486de', foundational, doctrine_suspension_under_duress_is_unchanged).
narrative_ontology:cs_axiom_status(doctrine_suspension_under_duress_is_unchanged, holdable).
narrative_ontology:cs_axiom_grounding('70596455-c449-4421-9daa-b5edd29486de', doctrine_suspension_under_duress_is_unchanged, deontological).
narrative_ontology:cs_reference_frame('70596455-c449-4421-9daa-b5edd29486de', prophetic_authority_as_divine_truth).
narrative_ontology:cs_drift_state('70596455-c449-4421-9daa-b5edd29486de', post_manifesto_federal_supremacy_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('70596455-c449-4421-9daa-b5edd29486de', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_ecclesiastical_hierarchy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, federal_legislative_authority).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, anti_polygamy_political_coalition).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__exogenous_override_reading, federal_supremacy_over_religious_practice).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__exogenous_override_reading, polygamy_illegitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises coercive force (territorial exclusion, property seizure, legal sanctions against polygamists) to compel institutional capitulation. The Manifesto is extracted under duress as evidence of compliance. The federal government collects the benefit of institutional submission and the vindication of federal supremacy doctrine: no religious institution can persist within the nation if its practices conflict with federal law, regardless of theological foundation.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% Bears the cost of doctrinal practice suspension: must publicly disown a core theological commitment under threat of institutional dissolution. The hierarchy makes the public declaration while maintaining (in this reading) that the underlying doctrine is unchanged—creating a permanent gap between material institutional behavior and claimed spiritual truth. Exit options are trapped: the institution cannot abandon the territory, the membership, or the institutional structure; it must capitulate or face confiscation and legal persecution.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_ecclesiastical_hierarchy, payer,
    powerful, generational, trapped, national).

% Carries the existential cost of the constraint: must navigate the gap between theological doctrine (as they understood it: eternal marriage as divine law) and suspended practice (polygamy forbidden). In this reading, the doctrine is theologically intact but practically suspended under external coercion—leaving members to internalize a permanent legitimacy crisis: Is the doctrine true or false? Was it revealed or was it fallible practice? The identity lock is acute: leaving the Church abandons kinship ties, social embeddedness, self-concept as covenant community member, and (in this reading) the claim to spiritual truth itself.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership, payer,
    organized, biographical, identity_locked, national).

% Collects the political benefit of demonstrating federal authority over religious institutions. Polygamy prohibition is vindicated as federal law, settable by legislation without religious exemption. The Manifesto is evidence that federal coercion works: religious hierarchy capitulates when threatened with institutional annihilation.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, federal_legislative_authority, beneficiary,
    institutional, generational, analytical, national).

% Gains the political victory: a religious institution deemed unfit for statehood is forced to surrender its core practice under federal pressure. The coalition's moral and legal framing (polygamy is inherently oppressive, federal law supersedes religious doctrine) is vindicated by the hierarchy's capitulation.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, anti_polygamy_political_coalition, beneficiary,
    organized, biographical, mobile, national).

% Are structurally excluded from the conversation: the hierarchy negotiates their abandonment without their voice. They bear the constraint's costs (legal prosecution, excommunication, family dissolution) but have no seat at the table where the hierarchy capitulates. Their exclusion is the extraction's structural mechanism: the hierarchy can make the deal only by silencing those whose lives instantiate the abandoned practice.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, polygamist_practitioners, excluded,
    powerless, biographical, trapped, national).

% Are doubly excluded: neither consulted by the hierarchy nor represented in federal legislative deliberation. They carry costs of both the original practice (if it was coercive, as critics allege) and its suspension (family dissolution, legal ambiguity, social stigma). In this reading's frame, their voice is absent from the coercion/consent narrative entirely.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, women_in_polygamist_families, excluded,
    powerless, biographical, trapped, local).

% Examines the structure: coercive federal pressure extracting institutional capitulation under threat. The constraint's persistence depends on the hierarchy's continued public compliance (no re-adoption of polygamy practice) and on the suppression of any public narrative that treats the doctrine as still-valid—the Manifesto's theological interpretation (doctrine unchanged vs. doctrine reversed) is the battleground where extraction is measured.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In this reading, there is no coordination function. The constraint is purely extractive: federal government imposes unilateral demand backed by coercive force. The LDS institution once coordinated marriage and kinship practice via claimed prophetic authority; the federal constraint does not solve a coordination problem—it transfers authority from religious to civil jurisdiction by force.
% TRANSFER_FUNCTION: Transfers institutional legitimacy and doctrinal authority from LDS leadership to federal supremacy. The hierarchy cedes public practice (and appearance of doctrinal acceptance) in exchange for institutional survival. The membership transfers from a Church claiming revelatory authority to one that (in this reading) performs false capitulation while maintaining unchanged doctrine privately.
% ABSENT_VOICES: Polygamist practitioners are structurally excluded: the hierarchy negotiates their abandonment without their input. Women in polygamist families are doubly absent—neither institutional partners nor federal legislative participants. Any dissenting voices within the hierarchy are suppressed by institutional discipline. Indigenous peoples whose labor and lands supported the LDS territorial system are entirely absent from the coercion narrative.
% DISAPPEARANCE_RATIONALE: If federal coercion ceased and the hierarchy declared polygamy theology reinstituted, the territorial and kinship structure of LDS institution would reorganize immediately. Federal enforcement would end, legal persecution would cease, and the institution could publicly re-assert its doctrine. The constraint's existence depends entirely on continuous federal enforcement and hierarchy compliance.
% FOUNDING_PROBLEM: From federal perspective: religious practices (polygamy) violate federal law and national moral norms; religious institutions must be subject to federal authority. From hierarchy perspective (in this reading): external political force threatens institutional survival unless the hierarchy publicly abandons core theology.
% FOUNDING_PROBLEM_CORROBORATION: Federal legislative record explicitly frames the founding problem as institutional defiance and federal supremacy requirement. LDS institutional records frame it as external coercion (in this reading). Non-participant historians document the federal coercive machinery (property seizure, legal persecution, territorial exclusion) and hierarchy capitulation under duress. Corroboration for the 'coercion' reading comes from historical documentation of coercive mechanisms outside both beneficiary seats.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__exogenous_override_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__exogenous_override_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at endpoint) because federal coercion compels institutional behavior that violates the hierarchy's stated core theology—the constraint persists only through continuous federal enforcement and hierarchy compliance, not through participant preference. Suppression is also high (0.76) because the constraint requires active enforcement (legal persecution of practitioners, institutional discipline of non-compliant members, suppression of any public narrative that treats doctrine as unchanged). Theater is moderate-to-high (0.58): the Manifesto itself is partially theatrical (claiming spiritual necessity when external coercion drove it) and institutional performance becomes increasingly central to constraint persistence—the hierarchy must continue appearing to accept the reversal while (in this reading) maintaining unchanged doctrine privately. Accessibility_collapse is moderate (0.71) because the federal supremacy doctrine creates a structural barrier (no religious institution can resist federal law) but alternatives (institutional relocation, doctrinal reframing, organizational exit) remain theoretically available, though practically foreclosed by institutional loyalty and identity lock. Resistance is high (0.68) because the hierarchy and membership demonstrate ongoing cognitive/spiritual resistance to the reversal (private maintenance of doctrine, dissent from silence, internal debate about legitimacy). The measurement series traces the escalation of coercion (1880–1890) and the subsequent plateau of enforced compliance (1890–1910), with theater_ratio rising as institutional performance becomes the primary mechanism of suppression.
 *
 * PERSPECTIVAL GAP:
 *   The federal government and hierarchy compute different constraint types from identical structural data. From the federal seat: this is legitimate institutional subordination to law (world_rearranges if federal supremacy is challenged; the constraint is rational enforcement). From the hierarchy's seat (in this reading): this is coerced extraction forcing false capitulation (world_rearranges if coercion ends and doctrine is re-asserted). From the membership's seat: this is a legitimacy crisis creating identity-lock suppression (world_rearranges if doctrine is re-asserted as true, or if members are released from obligation to it—both require institutional transformation). The engine computes directionality from the structural data: hierarchy and membership are targets (high d → high χ); federal government and legislative authority are beneficiaries (low d → low/negative χ). The perspectival gap emerges because different seats have different exit options, different power levels, and different theoretical understanding of what the doctrine means (federal supremacy vs. divine law vs. institutional identity).
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government: beneficiary seat (d ≈ 0.1–0.2). Extracts institutional compliance and vindication of supremacy doctrine. Exit is analytical (can exit framework at will); power is institutional. Effective extraction (χ) is amplified by scope (national) and anchored by beneficiary directionality. LDS ecclesiastical hierarchy: target seat (d ≈ 0.85–0.95). Bears the extraction cost directly (must publicly comply under duress). Exit is trapped (cannot abandon territory, membership, institutional structure without dissolution). Power is powerful but constrained by external coercion. Effective extraction is very high due to trap-level exit and target directionality. LDS membership: target seat (d ≈ 0.80–0.90). Bears suppression costs (identity-lock, legitimacy crisis, doctrinal uncertainty). Exit is identity_locked (leaving Church abandons kinship, community, self-concept as covenant member). Power is organized (collective voice) but coerced. Effective extraction is very high due to identity-lock exit and target directionality. Federal legislative authority: beneficiary seat (d ≈ 0.05–0.15). Collects political benefit of demonstrated federal supremacy. Exit is analytical; power is institutional. Extraction is low by directionality but amplified by scope. Anti-polygamy coalition: beneficiary seat (d ≈ 0.1–0.25). Collects moral and political victory. Exit is mobile (can disengage from coalition); power is organized. Extraction is moderate by directionality but anchored by beneficiary position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal supremacy over religious practice; institutional defiance of federal law) has a contested status in this reading. The federal perspective: founding problem is live (religious institutions continue to resist federal law and must be monitored). The hierarchy's perspective (in this reading): founding problem is dead (polygamy was abandoned and the federal coercion achieved its political goal). The membership's perspective: founding problem is contested (they dispute whether the 'problem' was genuine Mormon practice or federal overreach). The disappearance_verdict is world_rearranges because the constraint's persistence depends entirely on federal enforcement and hierarchy compliance—without the coercive threat, the hierarchy would immediately re-assert doctrinal integrity. The theater_ratio rising over the interval (0.25→0.58) indicates Goodhart drift: enforcement focus shifts from preventing polygamy practice (which largely ceased by 1890) to enforcing institutional performance (continuing public acceptance of doctrinal reversal). This is a classic mandatrophy signature: the founding purpose (suppress polygamy) is achieved by 1890; the constraint persists after that point primarily through hierarchical performance and suppression of dissent, not through functional necessity. The constraint does not resolve its mandate—it persists past mandate completion by shifting the maintenance burden from enforcement of practice to enforcement of narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrine_suspension_vs_reversal,
    'In the Manifesto, did the LDS hierarchy genuinely reverse its theological commitment to eternal marriage and polygamy, or did it suspend only practice while maintaining unchanged doctrine?',
    'This is the central axis dividing the three readings of the kernel. This reading (exogenous_override) asserts doctrine unchanged; the endogenous_reinterpretation reading asserts genuine doctrinal reversal by revelation; the hybrid reading holds scope ambiguity allows both. Resolution requires examining: (1) LDS leadership''s private statements and instructions to members; (2) the language of the Manifesto itself (does it claim divine revelation or institutional necessity); (3) subsequent institutional behavior (do LDS leaders maintain doctrinal commitment while suspending practice, or do they teach doctrinal reversal).',
    'If doctrine was genuinely unchanged, the constraint is a pure snare: external coercion extracting false appearance of capitulation. If doctrine was genuinely reversed, the constraint is an institutional adaptation with altered theological legitimacy. The type classification depends on this resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrine_suspension_vs_reversal, conceptual, 'Whether the theological reversal was genuine or feigned under duress.').

omega_variable(
    membership_consent_vs_coercion,
    'Did the LDS membership genuinely consent to doctrinal revision under the Manifesto, or do they experience it as coerced suspension they were never asked to approve?',
    'Post-Manifesto historical testimony from rank-and-file members, diaries, and oral histories. Also: did the hierarchy present the Manifesto as binding doctrinal reversal or as temporary political accommodation? What language was used in official communications to members?',
    'If membership was coerced without consent, the identity-lock on the membership seat is stronger and the suppression value is higher. If membership genuinely accepted it as divine will (via the endogenous_reinterpretation reading), the identity lock is weaker and the constraint operates more as institutional adaptation than snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(membership_consent_vs_coercion, empirical, 'Whether the membership''s acceptance was genuinely volitional or coerced.').

omega_variable(
    federal_coercion_mechanism_clarity,
    'What was the actual mechanism of federal coercion? Was it explicit threat of institutional dissolution, property seizure, legal persecution of members, or a combination? How directly did federal officials communicate the threat to LDS leadership?',
    'Historical documents: federal legislation (Edmunds Act, etc.), internal LDS correspondence with federal authorities, leadership statements to the hierarchy about what would happen without capitulation, contemporaneous legal actions against LDS institutions and members.',
    'If coercion was explicit and clearly communicated, this reading''s snare classification is reinforced. If coercion was implicit or ambiguous, the hierarchy''s capitulation becomes harder to frame as forced—leaving space for the endogenous_reinterpretation reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_coercion_mechanism_clarity, empirical, 'The specificity and clarity of federal threats.').

omega_variable(
    internalized_vs_structural_suppression,
    'Is the LDS membership''s suppression (their acceptance of doctrinal abandonment despite theological conflict) structural (external barriers: legal persecution, institutional pressure, property loss) or internalized (they came to believe the doctrine was wrong, or that obedience overrides doctrinal truth)?',
    'Post-Manifesto testimony and institutional rhetoric: did members describe external coercion (we were forced to give this up) or internal revelation (God commanded this reversal for wise purposes)? The dominant narrative in LDS institutional discourse is endogenous_reinterpretation (this reading claims the same population was supplied with exogenous narrative to manage legitimacy crisis). Examining lived suppression (identity-locked members'' own accounts) versus institutional narrative reveals the gap.',
    'Structural suppression (external barriers persist after behavioral change) suggests the constraint is a pure snare. Internalized suppression (members came to accept the doctrine as false or superseded) suggests institutional adaptation. Most likely: both are present and changing over generational time—early cohorts experienced structural suppression, later cohorts were socialized into acceptance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Suppression mechanism: structural coercion vs. internalized acceptance.').

omega_variable(
    reading_identity_in_committed_lineage,
    'This reading (exogenous_override) claims the doctrine is unchanged but practice suspended under federal coercion. Is this reading coherent within LDS theological self-understanding, or does it require external observer framing that LDS leadership cannot endorse?',
    'LDS official doctrine and leadership statements: does LDS teaching allow for (1) suspended doctrines that remain true but not binding, or (2) false doctrines revealed as false by new revelation? If (1), this reading is internally coherent to the tradition. If (2), this reading is an external observer framing that no LDS leader would defend (it would mean the 1890 Manifesto is a lie or political accommodation, not a divine statement).',
    'If this reading is coherent within LDS self-understanding, the kernel contest is between three live internal readings (endogenous_reinterpretation, exogenous_override, hybrid_pragmatic). If this reading is external observer framing only, the kernel contest is between endogenous_reinterpretation and hybrid_pragmatic, with exogenous_override as analytical framing outside the tradition''s own vocabulary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_in_committed_lineage, conceptual, 'Whether the exogenous_override reading is internally coherent to LDS doctrine or an external observer frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__exogenous_override_reading, 1880, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1880, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1880, 0.25).
narrative_ontology:measurement_basis(marr_tr_t1880, observed).
narrative_ontology:measurement(marr_tr_t1887, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1887, 0.38).
narrative_ontology:measurement_basis(marr_tr_t1887, observed).
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1890, 0.52).
narrative_ontology:measurement_basis(marr_tr_t1890, observed).
narrative_ontology:measurement(marr_tr_t1895, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1895, 0.56).
narrative_ontology:measurement_basis(marr_tr_t1895, observed).
narrative_ontology:measurement(marr_tr_t1900, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1900, 0.58).
narrative_ontology:measurement_basis(marr_tr_t1900, observed).
narrative_ontology:measurement(marr_tr_t1910, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1910, 0.58).
narrative_ontology:measurement_basis(marr_tr_t1910, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t1880, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1880, 0.45).
narrative_ontology:measurement_basis(marr_be_t1880, observed).
narrative_ontology:measurement(marr_be_t1887, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1887, 0.62).
narrative_ontology:measurement_basis(marr_be_t1887, observed).
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1890, 0.78).
narrative_ontology:measurement_basis(marr_be_t1890, observed).
narrative_ontology:measurement(marr_be_t1895, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1895, 0.81).
narrative_ontology:measurement_basis(marr_be_t1895, observed).
narrative_ontology:measurement(marr_be_t1900, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1900, 0.82).
narrative_ontology:measurement_basis(marr_be_t1900, observed).
narrative_ontology:measurement(marr_be_t1910, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1910, 0.82).
narrative_ontology:measurement_basis(marr_be_t1910, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1880, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1880, 0.35).
narrative_ontology:measurement_basis(marr_su_t1880, observed).
narrative_ontology:measurement(marr_su_t1887, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1887, 0.62).
narrative_ontology:measurement_basis(marr_su_t1887, observed).
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1890, 0.71).
narrative_ontology:measurement_basis(marr_su_t1890, observed).
narrative_ontology:measurement(marr_su_t1895, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1895, 0.74).
narrative_ontology:measurement_basis(marr_su_t1895, observed).
narrative_ontology:measurement(marr_su_t1900, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1900, 0.76).
narrative_ontology:measurement_basis(marr_su_t1900, observed).
narrative_ontology:measurement(marr_su_t1910, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1910, 0.76).
narrative_ontology:measurement_basis(marr_su_t1910, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1880, tn=1910
narrative_ontology:measurement(marr_grid_01, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(class), 1880, 0.55).
narrative_ontology:measurement(marr_grid_02, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(class), 1910, 0.72).
narrative_ontology:measurement(marr_grid_03, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(individual), 1880, 0.65).
narrative_ontology:measurement(marr_grid_04, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(individual), 1910, 0.78).
narrative_ontology:measurement(marr_grid_05, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(organizational), 1880, 0.35).
narrative_ontology:measurement(marr_grid_06, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(organizational), 1910, 0.88).
narrative_ontology:measurement(marr_grid_07, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(structural), 1880, 0.4).
narrative_ontology:measurement(marr_grid_08, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(structural), 1910, 0.85).
narrative_ontology:measurement(marr_grid_09, marriage_commitment_legitimacy__exogenous_override_reading, resistance(class), 1880, 0.65).
narrative_ontology:measurement(marr_grid_10, marriage_commitment_legitimacy__exogenous_override_reading, resistance(class), 1910, 0.35).
narrative_ontology:measurement(marr_grid_11, marriage_commitment_legitimacy__exogenous_override_reading, resistance(individual), 1880, 0.58).
narrative_ontology:measurement(marr_grid_12, marriage_commitment_legitimacy__exogenous_override_reading, resistance(individual), 1910, 0.38).
narrative_ontology:measurement(marr_grid_13, marriage_commitment_legitimacy__exogenous_override_reading, resistance(organizational), 1880, 0.81).
narrative_ontology:measurement(marr_grid_14, marriage_commitment_legitimacy__exogenous_override_reading, resistance(organizational), 1910, 0.12).
narrative_ontology:measurement(marr_grid_15, marriage_commitment_legitimacy__exogenous_override_reading, resistance(structural), 1880, 0.72).
narrative_ontology:measurement(marr_grid_16, marriage_commitment_legitimacy__exogenous_override_reading, resistance(structural), 1910, 0.18).
narrative_ontology:measurement(marr_grid_17, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(class), 1880, 0.52).
narrative_ontology:measurement(marr_grid_18, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(class), 1910, 0.71).
narrative_ontology:measurement(marr_grid_19, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(individual), 1880, 0.68).
narrative_ontology:measurement(marr_grid_20, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(individual), 1910, 0.74).
narrative_ontology:measurement(marr_grid_21, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(organizational), 1880, 0.48).
narrative_ontology:measurement(marr_grid_22, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(organizational), 1910, 0.88).
narrative_ontology:measurement(marr_grid_23, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(structural), 1880, 0.35).
narrative_ontology:measurement(marr_grid_24, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(structural), 1910, 0.82).
narrative_ontology:measurement(marr_grid_25, marriage_commitment_legitimacy__exogenous_override_reading, suppression(class), 1880, 0.48).
narrative_ontology:measurement(marr_grid_26, marriage_commitment_legitimacy__exogenous_override_reading, suppression(class), 1910, 0.68).
narrative_ontology:measurement(marr_grid_27, marriage_commitment_legitimacy__exogenous_override_reading, suppression(individual), 1880, 0.35).
narrative_ontology:measurement(marr_grid_28, marriage_commitment_legitimacy__exogenous_override_reading, suppression(individual), 1910, 0.72).
narrative_ontology:measurement(marr_grid_29, marriage_commitment_legitimacy__exogenous_override_reading, suppression(organizational), 1880, 0.22).
narrative_ontology:measurement(marr_grid_30, marriage_commitment_legitimacy__exogenous_override_reading, suppression(organizational), 1910, 0.81).
narrative_ontology:measurement(marr_grid_31, marriage_commitment_legitimacy__exogenous_override_reading, suppression(structural), 1880, 0.38).
narrative_ontology:measurement(marr_grid_32, marriage_commitment_legitimacy__exogenous_override_reading, suppression(structural), 1910, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__exogenous_override_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_legitimacy__exogenous_override_reading, 0.12).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the marriage_commitment_legitimacy kernel. The kernel is the LDS doctrinal claim about eternal marriage and celestial polygamy. The three readings differ on whether the Manifesto represents genuine revelation (endogenous), coerced suspension (exogenous), or strategic ambiguity (hybrid). Each reading instantiates a different constraint with a different ε, beneficiary/victim structure, and classification. The readings are linked because they all interpret the same historical event (the Manifesto) but locate the source of legitimacy differently: doctrine (endogenous), force (exogenous), or institution (hybrid). This reading (exogenous_override) understands the kernel authority as grounded in extraction: federal coercion overrides any doctrinal claim. The sibling readings understand it as grounded in lineage (endogenous) or practice (hybrid).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_legitimacy__exogenous_override_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
