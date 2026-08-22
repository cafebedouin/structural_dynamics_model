% ============================================================================
% CONSTRAINT STORY: biblical_source_text__critical_reconstructive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__critical_reconstructive_reading, []).

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
 *   constraint_id: biblical_source_text__critical_reconstructive_reading
 *   human_readable: Critical Reconstructive Reading of Biblical Source Text
 *   domain: religious/academic/hermeneutic
 *
 * SUMMARY:
 *   The critical-reconstructive reading claims that historical recovery of
 *   the hypothetical original text is the primary and methodologically
 *   legitimate goal of biblical study. Under this reading, neither the
 *   structure nor the meaning of the received text can be privileged until a
 *   reliable textual basis (the original) is established through critical
 *   apparatus, manuscript comparison, and philological reconstruction. This
 *   reading instantiates one framework within the contested
 *   biblical-source-text kernel. It benefits academic biblical scholarship
 *   (which gains irreplaceable mediating authority) while extracting
 *   epistemic destabilization from confessional faith communities (for whom
 *   the received text is the lived, transmitted, liturgical basis of faith).
 *   The measurement series tracks the constraint's intensification from its
 *   emergence in the 18th century through consolidation in academic
 *   institutions (1850–1920) to near-saturation in biblical studies curricula
 *   by 2000, with theater ratio rising as the original text becomes
 *   increasingly hypothetical while the scholarly apparatus becomes
 *   increasingly elaborate.
 *
 * KEY AGENTS:
 *   - Academic biblical scholars: Institutional beneficiary, agenda-setter, enforces historical-critical priority via peer review and hiring
 *   - Confessional faith communities: Organized victims, identity-locked (cannot exit without abandoning the faith tradition), experience epistemic destabilization
 *   - Textual criticism establishment: Institutional beneficiary, gatekeeps what counts as legitimate evidence and method
 *   - Lay textual recipients: Powerless victims, dependent on scholar-mediation they cannot evaluate
 *   - Alternative hermeneutic readings (formal/dynamic equivalence): Excluded competitors, structurally trapped outside academic authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, 0.68).
domain_priors:suppression_score(biblical_source_text__critical_reconstructive_reading, 0.71).
domain_priors:theater_ratio(biblical_source_text__critical_reconstructive_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__critical_reconstructive_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__critical_reconstructive_reading, "Critical Reconstructive Reading of Biblical Source Text").
narrative_ontology:topic_domain(biblical_source_text__critical_reconstructive_reading, "religious/academic/hermeneutic").

domain_priors:requires_active_enforcement(biblical_source_text__critical_reconstructive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__critical_reconstructive_reading, '161fef3c-cba8-4f40-82fd-208f4f2cf307').
narrative_ontology:cs_kernel_codification('161fef3c-cba8-4f40-82fd-208f4f2cf307', fixed_text).
narrative_ontology:cs_authority_grounding('161fef3c-cba8-4f40-82fd-208f4f2cf307', extraction).
narrative_ontology:cs_interpretation_layer_present('161fef3c-cba8-4f40-82fd-208f4f2cf307').
narrative_ontology:cs_reading_relation('161fef3c-cba8-4f40-82fd-208f4f2cf307', biblical_source_text__formal_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('161fef3c-cba8-4f40-82fd-208f4f2cf307', biblical_source_text__dynamic_equivalence_reading, coexists_with).
narrative_ontology:cs_axiom('161fef3c-cba8-4f40-82fd-208f4f2cf307', foundational, historical_original_epistemically_primary).
narrative_ontology:cs_axiom_status(historical_original_epistemically_primary, holdable).
narrative_ontology:cs_axiom_grounding('161fef3c-cba8-4f40-82fd-208f4f2cf307', historical_original_epistemically_primary, empirically_contingent).
narrative_ontology:cs_axiom('161fef3c-cba8-4f40-82fd-208f4f2cf307', foundational, textual_reconstruction_precondition_for_interpretation).
narrative_ontology:cs_axiom_status(textual_reconstruction_precondition_for_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('161fef3c-cba8-4f40-82fd-208f4f2cf307', textual_reconstruction_precondition_for_interpretation, instrumental).
narrative_ontology:cs_reference_frame('161fef3c-cba8-4f40-82fd-208f4f2cf307', historical_original_primacy).
narrative_ontology:cs_drift_state('161fef3c-cba8-4f40-82fd-208f4f2cf307', contemporary_postmodern_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('161fef3c-cba8-4f40-82fd-208f4f2cf307', '').
narrative_ontology:cs_kernel_id(biblical_source_text__critical_reconstructive_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, textual_criticism_establishment).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, confessional_faith_communities).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, lay_textual_recipients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the standard that historical reconstruction is the methodologically primary goal. Enforces this standard through peer review, accreditation, and hiring decisions. Collects professional prestige, research funding, and institutional gatekeeping power from maintaining this authority.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship, agenda_setter,
    institutional, generational, arbitrage, global).

% Experience sustained epistemic destabilization: their received text is rendered permanently suspect, requiring expert reconstruction to be understood. They cannot exit the constraint without abandoning their faith tradition entirely. They depend on scholar-mediators to interpret what their own texts mean.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, confessional_faith_communities, payer,
    organized, civilizational, identity_locked, global).

% Maintains methodological authority and professional autonomy by insisting textual reconstruction via apparatus and comparison is prior to interpretation. Retains gatekeeping power over what counts as legitimate evidence and method.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, textual_criticism_establishment, beneficiary,
    institutional, generational, arbitrage, global).

% Receive biblical texts mediated through academic-critical frameworks they cannot verify. They are told what they read is not what was originally written and must defer to scholarly reconstruction. They have no capacity to evaluate the reconstruction and no option to withdraw without being dismissed as intellectually unserious.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, lay_textual_recipients, payer,
    powerless, biographical, constrained, global).

% Formal-equivalence and dynamic-equivalence readings remain alive in religious publishing but are structurally excluded from institutional academic authority. Peer review and hiring standards make it costly to pursue alternative readings within universities, even where they might be defensible.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, alternative_hermeneutic_readings, excluded,
    moderate, generational, trapped, global).

% The historical record of biblical textual transmission. Framed unidirectionally by the constraint as a problem to be solved (recovering original text) rather than as a repository of legitimate forms. The evidence speaks only through the scholar's interpretive framing.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, manuscript_evidence_corpus, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(biblical_source_text__critical_reconstructive_reading, manuscript_evidence_corpus).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship).
narrative_ontology:fixing_cost_class(biblical_source_text__critical_reconstructive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes shared criteria for evaluating textual claims (manuscript evidence, comparative method, philological analysis). Enables biblical scholarship to operate as a cumulative discipline where experts build on prior work rather than each reader independently assessing texts.
% TRANSFER_FUNCTION: Moves epistemic authority from received textual communities (those inhabiting the text as transmitted) to academic reconstruction specialists. Also transfers labor: communities must employ scholars to interpret what their texts mean.
% ABSENT_VOICES: Confessional biblical scholars whose work emphasizes the cost of perpetual textual destabilization are systematically excluded from mainstream academic publication; practitioners of precritical hermeneutics (liturgical, mystical, typological reading) who would argue the constraint forecloses legitimate interpretive approaches; oral and vernacular text communities outside the academic frame who have no representation in scholarly discourse.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, academic biblical study would fragment into multiple legitimate methodological frameworks simultaneously. Professional authority would redistribute. Faith communities would recover confidence in their received texts. The institutional structure of biblical academia depends entirely on enforcing this constraint's priority.
% FOUNDING_PROBLEM: 18th–19th century: competing manuscript traditions, variant readings, and rising historical consciousness created genuine uncertainty about textual authenticity and original form. How do we adjudicate between variants and recover the earliest recoverable text?
% FOUNDING_PROBLEM_CORROBORATION: Academic textual critics attest the problem is live and unsolved (new manuscripts, new methods, perpetual reconstruction). Historians of the discipline (Levenson, Enns, reception historians) and confessional scholars attest the founding problem has been reframed: from 'which texts do we have and how do we use them?' to 'what institutional authority is required to interpret the texts for others?' The shift in framing is corroborated by external observers, not by the benefiting parties.
narrative_ontology:disappearance_verdict(biblical_source_text__critical_reconstructive_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__critical_reconstructive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__critical_reconstructive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_source_text__critical_reconstructive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__critical_reconstructive_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__critical_reconstructive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_source_text__critical_reconstructive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness runs high (0.68 at 2025) because the constraint transfers epistemic authority from communities to specialists and maintains that transfer through perpetual textual instability (the original text is always one step beyond reach, requiring ongoing expert reconstruction). Suppression is also high (0.71) because the constraint actively excludes alternative readings from institutional legitimacy via peer-review gatekeeping and accreditation standards. Theater ratio has climbed from negligible (0.05 in 1750 when the problem was genuinely ambiguous) to substantial (0.42 in 2025) as the constraint's function has shifted: initially a response to real textual uncertainty, it now performs authority maintenance even as the empirical problem it purports to solve has become less acute (we have more manuscripts, better methods, yet the original remains elusive and reconstructions proliferate). The temporal pattern shows a classic extraction ratchet: as the academic discipline matured and consolidated institutional power (1850–1970), both extractiveness and enforcement intensity climbed. By 2000, the curve flattened—the constraint reached saturation in academic settings and met increasing resistance (liberation theology scholars, feminist biblical criticism, confessional scholars, postcolonial readings all challenge the priority axiom), but suppression maintained extractiveness by keeping dissenting voices excluded from mainstream venues.
 *
 * PERSPECTIVAL GAP:
 *   From the academic seat, the constraint appears as methodological rigor: we must establish what the text originally said before we can responsibly interpret it. From the confessional seat, the same constraint appears as an imposed epistemic hierarchy that permanently destabilizes their received text and subordinates their interpretive authority to expert reconstruction. The engine computes this perspectival divergence from the structural data: agenda-setter (institutional power, arbitrage exit, collects authority) versus payer (organized but identity-locked, constrained exit, defers to specialist mediators). Neither seat can compute the other's type without altering power, exit, or beneficiary status—the divergence is structural, not observational.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic scholars are near-pure beneficiaries (d ≈ 0.15): they gain professional authority, funding, and gatekeeping power from the constraint; their exit options are excellent (they can choose among alternative readings, methodologies, or institutions); their time horizon is generational (institutional preservation). Confessional communities are near-pure targets (d ≈ 0.85): they bear the cost of epistemic destabilization; their exit is identity-locked (leaving the faith tradition to avoid the constraint is not a real option); their time horizon is civilizational (the tradition must continue). Lay recipients are trapped at high d (d ≈ 0.80): they cannot verify the reconstruction, cannot exit without accepting intellectual dismissal, and depend entirely on the scholar-mediator. The textual-criticism establishment sits at beneficiary-position d (d ≈ 0.10): they retain professional authority and epistemic priority. These divergent directionalities mean the constraint's effective extraction is amplified for the identity-locked targets and dampened (inverted to subsidy) for the institutional beneficiaries—which is exactly the asymmetry the constraint exists to maintain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (18th–19th century textual uncertainty) is nominally 'contested' by this story's authored verdict, but the more precise reading is: the problem has been reframed, not solved. Confessional scholars and historians of the discipline (Levenson, Enns, reception historians) argue that the constraint has transformed the problem from 'which variants do we have and how do we use them?' into 'what institutional authority is required to tell communities what their texts mean?' The original question could be solved (by comparative method and historical judgment); the reframed question cannot be solved (the original is perpetually reconstructible, never final, always requiring expert interpretation). This is a classic mandatrophy signature: the constraint persists because it no longer addresses the problem that justified it; it now addresses the perpetuation of the constraint's own function. The theater ratio climbing from 0.05 to 0.42 is the diagnostic: the function has migrated from solving a real empirical problem to performing scholarly authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_text_accessibility,
    'Is the ''original text'' of biblical manuscripts historically recoverable in principle, or is every reconstruction fundamentally underdetermined by the available evidence?',
    'Systematic comparison of manuscript-reconstruction methodologies across textual scholarship (biblical, classical, medieval). If different methods applied to the same manuscript set produce materially different ''original'' texts, the problem is underdetermination; if they converge within stable margins, the original is increasingly accessible.',
    'If underdetermined, the constraint''s claim that historical priority is methodologically primary becomes performative—it performs authority by declaring the problem solvable, when the evidence cannot settle it. The extraction mechanism becomes more visible: the constraint persists because it maintains the scholar''s irreplaceable role, not because it solves a genuine problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_text_accessibility, empirical, 'Whether the original biblical text is epistemically recoverable or fundamentally underdetermined by available evidence.').

omega_variable(
    committer_kernel_reading_identity,
    'Is the critical-reconstructive reading a coherent methodological stance, or does it conflate two distinct constraints: (a) textual reconstruction (a genuine coordination problem around manuscript evidence), and (b) interpretive authority (an institutional power claim that masks itself as methodological necessity)?',
    'Examine whether scholars who endorse the historical-reconstructive goal actually allow multiple legitimate readings of the reconstructed text, or whether they impose a single reading framework downstream of reconstruction. If the latter, the reading conflates reconstruction (empirical) with authority (institutional).',
    'If the two are conflated in the constraint''s structure, the reading should be decomposed into separate stories per the ε-invariance principle: one for textual reconstruction (lower extraction, genuine coordination), one for interpretive gatekeeping (higher extraction, institutional power). The current story would represent the gatekeeping reading, with a different beneficiary set and victim set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Whether this reading is a single ε-invariant constraint or a conflation of textual reconstruction and interpretive authority.').

omega_variable(
    confessional_resistance_trajectory,
    'Does measured resistance from confessional communities (0.72 at 2025) reflect genuine capacity to mount sustained institutional challenge, or performative/rhetorical resistance that the constraint''s enforcement machinery can readily suppress?',
    'Observe whether confessional scholarship produces peer-reviewed work in mainstream biblical journals, whether confessional hermeneutics are taught in accredited seminaries and universities alongside critical method, and whether hiring committees in biblical studies departments weight confessional perspectives as methodologically serious. If yes: institutional resistance is real. If no: resistance is present but suppressed by enforcement machinery.',
    'If suppressed-but-present, the constraint''s suppression metric (0.71) understates the enforcement burden required to maintain the hierarchy. The divergence between resistance and enforcement intensity would indicate that the constraint''s persistence depends on active institutional gatekeeping rather than voluntary consent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(confessional_resistance_trajectory, empirical, 'Whether confessional resistance is institutionally contained or suppressed.').

omega_variable(
    reading_family_stability,
    'Do the three sibling readings (critical-reconstructive, formal-equivalence, dynamic-equivalence) have stable, distinct ε values, or do they share an ε-invariant core with reading-indexed variations around it?',
    'Compare the extractiveness profiles: if formal-equivalence (prioritizing source-language structure) shows similarly high extraction for lay readers and similarly-low extraction for academic readers, the difference is reading-relative authority claims, not constraint ε. If formal-equivalence shows materially lower extraction overall (because it destabilizes fewer recipients), it is a genuinely different constraint with a different ε.',
    'If ε-invariant across readings, the three stories represent one constraint viewed from three authority framings—a single underlying power relationship wearing different hermeneutic justifications. If ε differs, they are three distinct constraints in a constraint family, linked by institutional competition rather than by epistemic choice. This omega determines whether the network.affects_constraints linkage is a family relationship or a contention relationship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_family_stability, conceptual, 'Whether the biblical-source-text kernel''s three readings are ε-invariant constraints or three distinct constraints with different extractiveness profiles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__critical_reconstructive_reading, 1750, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bst_crr_tr_t1750, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1750, 0.05).
narrative_ontology:measurement(bst_crr_tr_t1850, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1850, 0.12).
narrative_ontology:measurement(bst_crr_tr_t1920, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1920, 0.24).
narrative_ontology:measurement(bst_crr_tr_t1970, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1970, 0.38).
narrative_ontology:measurement(bst_crr_tr_t2000, biblical_source_text__critical_reconstructive_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(bst_crr_tr_t2025, biblical_source_text__critical_reconstructive_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(bst_crr_be_t1750, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1750, 0.22).
narrative_ontology:measurement(bst_crr_be_t1850, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1850, 0.45).
narrative_ontology:measurement(bst_crr_be_t1920, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1920, 0.58).
narrative_ontology:measurement(bst_crr_be_t1970, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement(bst_crr_be_t2000, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 2000, 0.67).
narrative_ontology:measurement(bst_crr_be_t2025, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bst_crr_su_t1750, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1750, 0.15).
narrative_ontology:measurement(bst_crr_su_t1850, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1850, 0.38).
narrative_ontology:measurement(bst_crr_su_t1920, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1920, 0.58).
narrative_ontology:measurement(bst_crr_su_t1970, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1970, 0.68).
narrative_ontology:measurement(bst_crr_su_t2000, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(bst_crr_su_t2025, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__critical_reconstructive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(biblical_source_text__critical_reconstructive_reading, 0.12).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, biblical_source_text__dynamic_equivalence_reading).

% DUAL FORMULATION NOTE:
% The biblical-source-text kernel contains three institutionally competing readings, each of which instantiates a different constraint on what counts as legitimate textual authority and interpretation. The critical-reconstructive reading (this story) claims historical recovery is primary; formal-equivalence prioritizes source-language structural fidelity; dynamic-equivalence prioritizes communicative effectiveness. These are not three perspectives on one constraint—they are three distinct constraints with three different beneficiary/victim structures and three different extractiveness profiles. They are linked via network.affects_constraints because institutional dominance of one reading suppresses the others' legitimacy. See commentary.kernel_context for the kernel contest details.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
