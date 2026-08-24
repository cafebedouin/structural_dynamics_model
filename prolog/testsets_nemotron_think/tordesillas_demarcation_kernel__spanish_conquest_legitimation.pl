% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__spanish_conquest_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tordesillas_demarcation_kernel__spanish_conquest_legitimation, []).

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
 *   constraint_id: tordesillas_demarcation_kernel__spanish_conquest_legitimation
 *   human_readable: Papal Grant as License for Spanish Conquest and Indigenous Subjugation West of Tordesillas Line
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   The 1493 papal bull Inter caetera and the 1494 Treaty of Tordesillas
 *   established a meridian dividing the non-Christian world between Spain and
 *   Portugal. The Spanish reading treats this grant as a license for
 *   territorial conquest and indigenous subjugation west of the line. The
 *   constraint operates through the encomienda system (grants of indigenous
 *   labor), the Requerimiento (a legalistic demand for submission read before
 *   attack), forced conversion, and the quinto real (crown's 20% of precious
 *   metals). The claimed type is snare: high extraction, active suppression,
 *   identifiable victims (indigenous populations), and coordination story
 *   (Christianization, conflict prevention) serving as cover. The metrics
 *   reflect the acceleration from initial Caribbean footholds (1494-1505)
 *   through the conquests of Aztec (1519-21) and Inca (1532-33) empires to
 *   the consolidation of viceregal administration and the New Laws debate
 *   (1542-50).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.88).
domain_priors:suppression_score(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.92).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, extractiveness, 0.88).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, snare).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "Papal Grant as License for Spanish Conquest and Indigenous Subjugation West of Tordesillas Line").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__spanish_conquest_legitimation, '864748dd-7a52-40ae-afc8-91cd614569a3').
narrative_ontology:cs_kernel_codification('864748dd-7a52-40ae-afc8-91cd614569a3', formalized).
narrative_ontology:cs_authority_grounding('864748dd-7a52-40ae-afc8-91cd614569a3', extraction).
narrative_ontology:cs_interpretation_layer_present('864748dd-7a52-40ae-afc8-91cd614569a3').
narrative_ontology:cs_reading_relation('864748dd-7a52-40ae-afc8-91cd614569a3', tordesillas_demarcation_kernel__portuguese_exploration_legitimation, coexists_with).
narrative_ontology:cs_axiom('864748dd-7a52-40ae-afc8-91cd614569a3', foundational, papal_grant_conveys_temporal_dominion).
narrative_ontology:cs_axiom_status(papal_grant_conveys_temporal_dominion, holdable).
narrative_ontology:cs_axiom_grounding('864748dd-7a52-40ae-afc8-91cd614569a3', papal_grant_conveys_temporal_dominion, theological).
narrative_ontology:cs_axiom('864748dd-7a52-40ae-afc8-91cd614569a3', secondary, christianization_mandates_conquest).
narrative_ontology:cs_axiom_status(christianization_mandates_conquest, holdable).
narrative_ontology:cs_axiom_grounding('864748dd-7a52-40ae-afc8-91cd614569a3', christianization_mandates_conquest, theological).
narrative_ontology:cs_reference_frame('864748dd-7a52-40ae-afc8-91cd614569a3', papal_universal_monarchy_1493).
narrative_ontology:cs_drift_state('864748dd-7a52-40ae-afc8-91cd614569a3', valladolid_debate_1550, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('864748dd-7a52-40ae-afc8-91cd614569a3', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_conquistadors).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_church_in_americas).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_conquistadors).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, papal_authority_over_temporal_dominion).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, christianization_justifies_conquest).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, terra_nullius_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Indies through the Council of the Indies, issues encomienda grants, appoints viceroys and audiencias. Collects the quinto real (20% of all precious metals) and controls trade via the Casa de Contratación. The papal bulls (Inter caetera, Eximiae devotionis) and Treaty of Tordesillas are the foundational legal titles cited in all royal decrees. Exit is not a consideration — the administration is the architect of the constraint.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration, agenda_setter,
    institutional, generational, arbitrage, global).

% Lead expeditions at personal cost, receive encomiendas (grants of indigenous labor and tribute) in return for conquest and pacification. Benefit from the legal framework that converts military action into heritable property rights over people and land. Also bear costs: high mortality, capital risk, and crown oversight that can revoke grants. Their exit is constrained by sunk investment and the lack of alternative paths to nobility and wealth.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_conquistadors, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_conquistadors, payer).

% Receives the spiritual mandate to convert indigenous souls, which translates into vast land holdings (doctrinas, misiones), tithes, and control over education and social life. The papal grant legitimizes the Church's institutional presence and its claim to a monopoly on religious life. Exit is identity-locked: the Church's self-conception as the universal vehicle of salvation makes the mandate inseparable from its institutional identity.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_church_in_americas, beneficiary,
    institutional, generational, identity_locked, global).

% Subject to conquest, forced relocation, encomienda labor drafts, tribute demands, and compulsory conversion. Their pre-existing political structures (tlatoque, curacas, caciques) are either co-opted or destroyed. No legal standing in Spanish courts to challenge the title; the Requerimiento is read as a performative notice before attack. Exit is trapped: geographic containment, demographic collapse from disease, and military asymmetry leave no viable alternative.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line, payer,
    powerless, generational, trapped, continental).

% Holds the sibling reading of the same kernel (portuguese_exploration_legitimation). The Treaty of Tordesillas assigns Brazil (east of line) to Portugal, but the Spanish reading's expansive interpretation of the papal grant as universal license creates persistent boundary disputes. Portugal cannot exit the constraint system — it is a party to the treaty — but its exit options are constrained by the need to maintain the treaty framework while contesting Spanish overreach.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, portuguese_crown, excluded,
    powerful, generational, constrained, global).

% Figures like Francisco de Vitoria and Domingo de Soto analyze the constraint from natural law. Vitoria's De Indis (1539) argues the papal grant cannot convey temporal dominion over non-Christians; the Indians are true owners (dominium) and the Spanish title rests on just war or voluntary acceptance only. Their analysis does not change the constraint's operation but creates the intellectual record that later international law draws on.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, school_of_salamanca_jurists, observer,
    organized, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates European territorial claims in the Americas under papal authority to prevent intra-European conflict over newly encountered lands.
% TRANSFER_FUNCTION: Moves land, labor, sovereignty, and mineral wealth from indigenous populations to Spanish Crown and colonists via encomienda grants, the quinto real, forced conversion, and the Requerimiento ritual.
% ABSENT_VOICES: Indigenous populations had no representation in the treaty negotiation or the papal curia; their sovereignty concepts and property systems were not recognized as valid. African populations later brought as enslaved labor were equally absent. The voices of Spanish reformers (Las Casas, Vitoria) were present in metropolitan debate but structurally excluded from colonial administration.
% DISAPPEARANCE_RATIONALE: If the papal grant and Tordesillas treaty vanished overnight, the Spanish Crown's legal title to the Indies would collapse in European law. Other European powers (France, England, Netherlands) would immediately contest possession. Indigenous polities would retain recognized sovereignty under natural law. The entire administrative apparatus (viceroyalties, audiencias, encomiendas) would lose its foundational legitimization.
% FOUNDING_PROBLEM: Prevent war between Spain and Portugal over Atlantic discoveries and provide a legal framework for the Christianization of newly encountered peoples.
% FOUNDING_PROBLEM_CORROBORATION: Spanish Crown and Church attested the Christianization mandate as live; indigenous voices and the School of Salamanca jurists (Vitoria, Las Casas) contested the conquest legitimization from outside the beneficiary set. The Valladolid debate (1550-51) and the New Laws (1542) show the founding problem was recognized as shifted by contemporaries not benefiting from the arrangement.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__spanish_conquest_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__spanish_conquest_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.88) is very high because the constraint transfers nearly all surplus (land, labor, mineral wealth, sovereignty) from indigenous populations to Spanish institutions. Suppression (0.92) is near-total: military conquest, demographic collapse from disease, legal non-personhood in Spanish courts, and the Requerimiento ritual eliminate alternatives. Theater ratio (0.42) is moderate — the Christianization mandate is genuinely believed by many actors but increasingly functions as cover for extraction. Accessibility collapse (0.89) reflects the near-total elimination of indigenous political autonomy. Resistance (0.78) is high — sustained indigenous rebellions (Mixtón, Chichimeca, Mapuche) and metropolitan legal challenges (Vitoria, Las Casas) meet the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the Spanish Crown's seat, the constraint is a legitimate exercise of papal-delegated authority that brings order and salvation. From the indigenous seat, it is catastrophic dispossession enforced by violence and disease. From the Salamanca jurists' seat, it is a legally dubious claim that violates natural law. The engine computes these divergences from the structural data; the authored claim (snare) reflects the indigenous and jurist perspectives, while the Crown's own framing would claim rope or scaffold.
 *
 * DIRECTIONALITY LOGIC:
 *   Spanish colonial administration is the structural beneficiary (d ≈ 0.05) — it designs, administers, and collects from the constraint. Conquistadors are beneficiaries with secondary payer role (d ≈ 0.25) — they receive encomiendas but bear campaign risks and crown oversight. Catholic Church in Americas is a beneficiary (d ≈ 0.15) — receives land, tithes, spiritual monopoly, but is identity-locked to the mandate. Indigenous populations are full targets (d ≈ 0.98) — trapped, powerless, bearing all extraction. Portuguese crown is excluded (d ≈ 0.6) — a co-signatory to the treaty but disadvantaged by the Spanish reading's expansive interpretation. Salamanca jurists are analytical observers (d ≈ 0.5) — symmetric costs/benefits, analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing Iberian war, Christianizing) was substantially achieved by 1529 (Treaty of Zaragoza settles the Moluccas; mass baptisms underway). Yet the extraction machinery (encomienda, quinto, forced labor) intensified after the founding problem faded — classic mandatrophy. The New Laws (1542) attempted to curtail encomienda but were partially rolled back under colonial pressure, showing the constraint's extraction function now drives its persistence. The constraint is a snare whose coordination cover has atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_temporal_authority_scope,
    'Does the papal grant convey temporal dominion (land, labor, sovereignty) or only spiritual jurisdiction (conversion, ecclesiastical organization) over non-Christian peoples?',
    'Textual analysis of Inter caetera (1493) and subsequent bulls; the School of Salamanca''s natural law critique (Vitoria De Indis 1539); the practical operation of the Patronato Real which fused spiritual and temporal administration.',
    'If only spiritual, the Spanish Crown''s temporal title collapses and the constraint is pure usurpation (snare with zero coordination legitimacy). If temporal, the constraint retains a thin coordination claim (preventing Iberian war) but the extraction vastly exceeds any papal mandate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(papal_temporal_authority_scope, conceptual, 'Scope of papal authority — spiritual vs temporal — as the structural pivot between coordination and pure extraction.').

omega_variable(
    indigenous_sovereignty_recognition,
    'Did the treaty system recognize any residual sovereignty or property rights in indigenous polities, or was terra nullius the operative doctrine from inception?',
    'Comparison of Spanish legal practice (Requerimiento, capitulaciones, New Laws) with Portuguese practice in Brazil (sesmarias, aldeamentos); Vitoria''s argument that dominium belongs to indigenous true owners; the 1537 bull Sublimis Deus which affirmed indigenous rationality and capacity for faith.',
    'If indigenous sovereignty was recognized, the constraint''s extraction includes a theft of recognized title (strengthening snare classification). If terra nullius was operative, the extraction is framed as original acquisition (still extractive but with different legal genealogy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_sovereignty_recognition, empirical, 'Whether indigenous political and property rights were ever recognized within the constraint''s own legal framework.').

omega_variable(
    kernel_reading_boundary,
    'Where exactly does the structural boundary lie between the spanish_conquest_legitimation and portuguese_exploration_legitimation readings of the same kernel?',
    'Comparative analysis of how each Crown''s jurists cited the same treaty articles; the 1529 Treaty of Zaragoza negotiations; the divergent colonial administrative structures (Spanish viceroyalties vs Portuguese donatary captaincies).',
    'If the readings are merely territorial partitions of the same extraction logic, they form a single constraint family with shared extractive structure. If they are genuinely different constraint types (snare vs rope), the kernel decomposition is analytically necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural relationship between the two sibling readings — whether they share extractive architecture or differ in kind.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 1494, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tordesillas_spanish_tr_t1494, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1494, 0.15).
narrative_ontology:measurement(tordesillas_spanish_tr_t1505, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1505, 0.22).
narrative_ontology:measurement(tordesillas_spanish_tr_t1519, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1519, 0.31).
narrative_ontology:measurement(tordesillas_spanish_tr_t1532, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1532, 0.38).
narrative_ontology:measurement(tordesillas_spanish_tr_t1542, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1542, 0.41).
narrative_ontology:measurement(tordesillas_spanish_tr_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1550, 0.42).

% Extraction over time
narrative_ontology:measurement(tordesillas_spanish_be_t1494, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1494, 0.35).
narrative_ontology:measurement(tordesillas_spanish_be_t1505, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1505, 0.52).
narrative_ontology:measurement(tordesillas_spanish_be_t1519, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1519, 0.71).
narrative_ontology:measurement(tordesillas_spanish_be_t1532, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1532, 0.83).
narrative_ontology:measurement(tordesillas_spanish_be_t1542, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1542, 0.86).
narrative_ontology:measurement(tordesillas_spanish_be_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1550, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(tordesillas_spanish_su_t1494, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1494, 0.45).
narrative_ontology:measurement(tordesillas_spanish_su_t1505, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1505, 0.62).
narrative_ontology:measurement(tordesillas_spanish_su_t1519, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1519, 0.78).
narrative_ontology:measurement(tordesillas_spanish_su_t1532, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1532, 0.88).
narrative_ontology:measurement(tordesillas_spanish_su_t1542, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1542, 0.9).
narrative_ontology:measurement(tordesillas_spanish_su_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1550, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__spanish_conquest_legitimation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.12).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel__portuguese_exploration_legitimation).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, new_laws_1542_indigenous_protection).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomienda_system_americas).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, requerimiento_ritual).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, patronato_real_americas).

% DUAL FORMULATION NOTE:
% The tordesillas_demarcation_kernel decomposes into two constraint stories: spanish_conquest_legitimation (this story, high-epsilon snare) and portuguese_exploration_legitimation (sibling, lower-epsilon rope/tangled_rope). They share the same treaty text but instantiate different constraints with different beneficiary/victim structures, extractiveness, and operational logics. The Spanish reading extracts from indigenous populations via conquest; the Portuguese reading primarily coordinates European exclusion east of the line with extraction focused on trade monopolies (not territorial conquest of sovereign polities at the same scale).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
