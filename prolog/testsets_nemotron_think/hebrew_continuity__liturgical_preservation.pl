% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__liturgical_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__liturgical_preservation, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: hebrew_continuity__liturgical_preservation
 *   human_readable: Hebrew Vitality Through Liturgical Preservation
 *   domain: sociolinguistic/religious/commitment_system
 *
 * SUMMARY:
 *   The liturgical_preservation reading of the hebrew_continuity kernel
 *   asserts that Hebrew 'lives' not through native speakers or daily use, but
 *   through the fidelity of ritual recitation and textual transmission across
 *   generations. This reading emerged from the rabbinic response to the loss
 *   of spoken Hebrew c. 200 CE: the text became the homeland. The constraint
 *   coordinates dispersed communities around a fixed textual center,
 *   requiring massive educational investment but zero native speakers. It
 *   extracts interpretive authority and educational labor from laity to
 *   rabbinic institutions. The claimed type is tangled_rope: genuine
 *   coordination (diaspora unity without territory) plus asymmetric
 *   extraction (rabbinic monopoly on textual authority, lay burden of
 *   non-generative learning). The victim set in this reading's own framing is
 *   'secularizing forces threatening textual tradition,' but structurally the
 *   payers are assimilating and secular Jews who bear alienation and
 *   educational costs without the coordination benefit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, 0.62).
domain_priors:suppression_score(hebrew_continuity__liturgical_preservation, 0.55).
domain_priors:theater_ratio(hebrew_continuity__liturgical_preservation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, extractiveness, 0.62).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__liturgical_preservation, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__liturgical_preservation, "Hebrew Vitality Through Liturgical Preservation").
narrative_ontology:topic_domain(hebrew_continuity__liturgical_preservation, "sociolinguistic/religious/commitment_system").

domain_priors:requires_active_enforcement(hebrew_continuity__liturgical_preservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__liturgical_preservation, '8d180be0-8dc3-48c1-9d48-f73810e8d946').
narrative_ontology:cs_kernel_codification('8d180be0-8dc3-48c1-9d48-f73810e8d946', fixed_text).
narrative_ontology:cs_authority_grounding('8d180be0-8dc3-48c1-9d48-f73810e8d946', lineage).
narrative_ontology:cs_interpretation_layer_present('8d180be0-8dc3-48c1-9d48-f73810e8d946').
narrative_ontology:cs_reading_relation('8d180be0-8dc3-48c1-9d48-f73810e8d946', hebrew_continuity__native_generative, forecloses).
narrative_ontology:cs_reading_relation('8d180be0-8dc3-48c1-9d48-f73810e8d946', hebrew_continuity__bridge_pidginized, influences).
narrative_ontology:cs_axiom('8d180be0-8dc3-48c1-9d48-f73810e8d946', foundational, hebrew_lives_through_textual_fidelity).
narrative_ontology:cs_axiom_status(hebrew_lives_through_textual_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('8d180be0-8dc3-48c1-9d48-f73810e8d946', hebrew_lives_through_textual_fidelity, conventional).
narrative_ontology:cs_axiom('8d180be0-8dc3-48c1-9d48-f73810e8d946', secondary, native_speakers_unnecessary_for_vitality).
narrative_ontology:cs_axiom_status(native_speakers_unnecessary_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('8d180be0-8dc3-48c1-9d48-f73810e8d946', native_speakers_unnecessary_for_vitality, conventional).
narrative_ontology:cs_reference_frame('8d180be0-8dc3-48c1-9d48-f73810e8d946', textual_tradition_authority).
narrative_ontology:cs_drift_state('8d180be0-8dc3-48c1-9d48-f73810e8d946', post_haskalah_secularization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8d180be0-8dc3-48c1-9d48-f73810e8d946', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__liturgical_preservation, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, traditional_communities).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, assimilating_jews).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, secular_jews_pressured).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, traditional_communities).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, secular_modern_jews).
narrative_ontology:constraint_vindicates(hebrew_continuity__liturgical_preservation, textual_fidelity_constitutes_language_vitality).
narrative_ontology:constraint_vindicates(hebrew_continuity__liturgical_preservation, native_speakers_unnecessary_for_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the canonical text, its vocalization, and interpretive tradition; authorize who may transmit and teach; define what counts as correct recitation. Their authority derives from the claimed unbroken chain of transmission. They bear minimal personal cost while collecting status, institutional control, and interpretive monopoly.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, rabbinic_authorities, agenda_setter,
    institutional, generational, analytical, universal).

% Gain collective identity cohesion, ritual participation, and continuity with ancestors through shared liturgical practice. Simultaneously bear massive educational costs: years of textual study for males, maintenance of educational infrastructure, social pressure to maintain ritual competence. Exit means leaving the community and its support networks.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, traditional_communities, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, traditional_communities, payer).

% Experience the constraint as alienating pressure: liturgical Hebrew functions as a gatekeeping boundary marking them as deficient Jews. They bear costs of exclusion from ritual leadership, communal legitimacy, and textual ownership without the compensatory benefit of identity cohesion. Can exit by disengaging from traditional institutions, but pay identity costs.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, secular_modern_jews, payer,
    organized, biographical, mobile, global).

% Caught between the liturgical framework's demands and surrounding majority culture. Bear the full educational burden without the communal reinforcement that traditional communities provide. The constraint extracts time, cognitive effort, and cultural capital while offering no generative fluency payoff. Exit is structurally constrained by family and communal expectations.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, assimilating_jews, payer,
    moderate, biographical, constrained, global).

% Native speakers of revived spoken Hebrew in Israel. The liturgical_preservation reading treats their native fluency as irrelevant to — or even a distraction from — 'true' Hebrew vitality. They are excluded from the constraint's definition of the kernel's life. They would contest the claim that Hebrew lives only in liturgy, but have no voice in the rabbinic interpretive structure.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, modern_hebrew_speakers, excluded,
    powerful, biographical, arbitrage, national).

% Study the transmission history, sociolinguistics, and competing vitality claims from outside the commitment system. They see the full structural field: the coordination function, the extraction, the excluded alternatives. Their analysis carries no enforcement power within the system.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, academic_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains Jewish collective identity and textual continuity across diaspora and centuries without requiring a shared spoken vernacular or territorial base. The fixed liturgical text provides a stable coordination point that survives migration, persecution, and language shift.
% TRANSFER_FUNCTION: Moves interpretive authority and educational burden from laity to rabbinic authorities; moves communal resources (time, money, cognitive effort) into textual education and ritual maintenance. The laity pays in learning costs; the rabbinate collects in authority and institutional control.
% ABSENT_VOICES: Secularizing Jews, early Zionists, modern Hebrew speakers, and Jewish diaspora communities that adopted vernacular languages — all who would argue Hebrew lives through spoken use, not textual preservation. They are structurally excluded because the liturgical framework defines 'Hebrew life' in terms that render their experience invisible.
% DISAPPEARANCE_RATIONALE: If the liturgical preservation constraint vanished overnight, traditional communities would lose their primary textual anchor and coordination mechanism across diaspora. Rabbinic authority would lose its textual basis. New coordination mechanisms (spoken Hebrew, secular culture, nationalist identity) would need to absorb the cohesion function — a massive rearrangement of Jewish collective life.
% FOUNDING_PROBLEM: How to maintain Jewish textual and ritual unity across diaspora after the loss of spoken Hebrew (c. 200 CE), the Temple's destruction, and the dispersion of communities with no shared vernacular?
% FOUNDING_PROBLEM_CORROBORATION: Historical record confirms diaspora communities used liturgical Hebrew as primary unity mechanism for 1700+ years (outside corroboration: Christian Hebraists, Islamic polemicists, traveler accounts). But Zionist historians (Ben-Yehuda, Fellman) and sociolinguists (Fishman, Spolsky) document the founding problem as substantially resolved by spoken revival in Palestine/Israel — a corroboration from outside the beneficiary set that the problem is dead or transformed.
narrative_ontology:disappearance_verdict(hebrew_continuity__liturgical_preservation, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__liturgical_preservation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__liturgical_preservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_continuity__liturgical_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__liturgical_preservation, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__liturgical_preservation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__liturgical_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the decoupling of educational burden from generative payoff: communities invest in textual mastery that yields ritual competence but not spoken fluency. Suppression (0.55) is moderate: alternatives (spoken Hebrew, secular Jewish culture, Zionist revival) are not banned but are delegitimized within the framework as 'not real Hebrew life.' Theater ratio (0.28) is low-moderate: the ritual function is genuine and valued, but a growing share of enforcement energy defends textual monopoly against modern challengers. Accessibility collapse (0.78) is high: once you accept the liturgical frame, spoken fluency becomes irrelevant to 'Hebrew vitality.' Resistance (0.58) reflects 150 years of Haskalah, Zionism, and secularization contesting the frame.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic seat, this is a rope: pure coordination solving the diaspora unity problem. From the secular Jew seat, it is a snare: extraction of identity legitimacy without reciprocity. From the traditional community seat, it is a tangled rope: the coordination is real and valued, but the extraction (educational burden, interpretive monopoly) is experienced as the price of belonging. The engine computes this divergence; the authored claim (tangled_rope) reflects the analyst's structural judgment that both coordination and extraction are substantively present.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities are structural beneficiaries (d ≈ 0.15): they collect authority, status, and interpretive monopoly with minimal cost. Traditional communities are dual-positioned: beneficiaries of coordination (d ≈ 0.4) but payers of educational extraction (d ≈ 0.7). Secular/modern Jews and assimilating Jews are targets (d ≈ 0.8): they bear alienation and exclusion costs. Modern Hebrew speakers are excluded (d not computed): the constraint's framework renders their native fluency structurally invisible. The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diaspora textual unity without spoken Hebrew) has been substantially resolved by the spoken Hebrew revival — but the constraint persists and expands. This is mandatrophy: the arrangement's original coordination function has been partially superseded by a more effective solution (native generative use), yet the liturgical apparatus maintains itself through institutional inertia and identity fusion. The constraint is not a piton because the coordination function remains live for traditional communities; it is a tangled rope whose extraction component has grown relative to its coordination necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_liturgical_preservation,
    'This constraint is one reading (liturgical_preservation) of the contested hebrew_continuity kernel. How does the reading''s structural classification change if a different reading (native_generative or bridge_pidginized) is instantiated instead?',
    'Generate sibling constraint stories for native_generative and bridge_pidginized readings; compare their ε, beneficiary/victim structures, and computed types. The kernel-level analysis requires the family of readings.',
    'If native_generative computes as mountain or rope (low extraction), the liturgical_preservation reading''s tangled_rope classification reflects a specific institutional form, not Hebrew continuity per se. If bridge_pidginized also shows extraction, the pattern is systemic to commitment-system Hebrew vitality claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_liturgical_preservation, conceptual, 'Committee-frame: this story is one reading of a contested kernel; classification is reading-indexed.').

omega_variable(
    suppression_mechanism_liturgical,
    'Is the suppression of alternative Hebrew vitality claims (spoken fluency, secular culture) structural (institutional control of education, conversion, marriage) or internalized (identity fusion where ''good Jew'' = liturgical competence)?',
    'Post-exit trajectory study: do Jews who leave traditional communities retain the belief that their Hebrew is ''not real''? If suppression persists after institutional pressure ends, it is partially internalized.',
    'If internalized, effective suppression is higher than structural measure — the constraint travels with the agent after exit. This would raise χ for payer seats beyond what structural d predicts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_liturgical, empirical, 'Structural vs. internalized suppression in identity-fused constraint.').

omega_variable(
    coordination_extraction_boundary_liturgical,
    'Is the massive educational burden of liturgical mastery a genuine coordination cost (the price of maintaining a shared textual standard across millennia) or extractive overhead (rabbinic monopoly inflating the burden beyond what coordination requires)?',
    'Compare educational investment in liturgical Hebrew vs. other diaspora textual traditions (Qur''anic Arabic, Classical Chinese, Sanskrit) controlling for community size and dispersion. Excess burden beyond cross-cultural baseline indicates extraction.',
    'If coordination cost, ε should be lower; the current 0.62 overstates extraction. If extractive overhead, ε is accurate or understated. This is the central ε-invariance test for this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_liturgical, empirical, 'Whether educational burden is coordination necessity or rent-seeking inflation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__liturgical_preservation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hclp_tr_t0, hebrew_continuity__liturgical_preservation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hclp_tr_t20, hebrew_continuity__liturgical_preservation, theater_ratio, 20, 0.18).
narrative_ontology:measurement(hclp_tr_t40, hebrew_continuity__liturgical_preservation, theater_ratio, 40, 0.22).
narrative_ontology:measurement(hclp_tr_t60, hebrew_continuity__liturgical_preservation, theater_ratio, 60, 0.25).
narrative_ontology:measurement(hclp_tr_t80, hebrew_continuity__liturgical_preservation, theater_ratio, 80, 0.27).
narrative_ontology:measurement(hclp_tr_t100, hebrew_continuity__liturgical_preservation, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(hclp_be_t0, hebrew_continuity__liturgical_preservation, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(hclp_be_t20, hebrew_continuity__liturgical_preservation, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(hclp_be_t40, hebrew_continuity__liturgical_preservation, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(hclp_be_t60, hebrew_continuity__liturgical_preservation, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(hclp_be_t80, hebrew_continuity__liturgical_preservation, base_extractiveness, 80, 0.61).
narrative_ontology:measurement(hclp_be_t100, hebrew_continuity__liturgical_preservation, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hclp_su_t0, hebrew_continuity__liturgical_preservation, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(hclp_su_t20, hebrew_continuity__liturgical_preservation, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(hclp_su_t40, hebrew_continuity__liturgical_preservation, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(hclp_su_t60, hebrew_continuity__liturgical_preservation, suppression_requirement, 60, 0.52).
narrative_ontology:measurement(hclp_su_t80, hebrew_continuity__liturgical_preservation, suppression_requirement, 80, 0.54).
narrative_ontology:measurement(hclp_su_t100, hebrew_continuity__liturgical_preservation, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__liturgical_preservation, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__liturgical_preservation, 0.08).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__native_generative).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% This is the liturgical_preservation reading of the hebrew_continuity kernel. The native_generative reading (Hebrew lives only through native speakers) and bridge_pidginized reading (Hebrew lives as diaspora contact language) are sibling constraints. This reading forecloses native_generative (contradictory core premises) and influences bridge_pidginized (provides textual reservoir). All three form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_continuity__liturgical_preservation, institutional, 0.15).
constraint_indexing:directionality_override(hebrew_continuity__liturgical_preservation, organized, 0.55).
constraint_indexing:directionality_override(hebrew_continuity__liturgical_preservation, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
