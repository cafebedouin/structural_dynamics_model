% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__spanish_conquest_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: tordesillas_demarcation_kernel__spanish_conquest_legitimation
 *   human_readable: Papal Grant License for Conquest and Indigenous Subjugation (Spanish Reading)
 *   domain: international law / colonial history / sovereignty
 *
 * SUMMARY:
 *   The Treaty of Tordesillas (1494) and the papal bulls framing it (Inter
 *   Caetera) granted the Spanish crown authorization to conquer, settle, and
 *   subjugate all territories west of a demarcation line drawn through the
 *   Atlantic and Indian Oceans. This constraint story instantiates the
 *   Spanish reading: that the papal grant functions as a divine license for
 *   conquest and indigenous subjugation, justified through Christian
 *   conversion mandate. The indigenous populations had no voice in the
 *   negotiation, no path to exit the constraint, and no appeal to a higher
 *   authority that recognized their sovereignty. The constraint operated
 *   through Spanish fleets, colonial administrators, encomienda labor
 *   systems, and missionary enforcement. It persists through appeals to papal
 *   authority and Christian doctrine, both of which remain binding on Spanish
 *   institutional legitimacy during the interval studied (1494–1750). The
 *   ε-invariance principle applies: this reading assesses the standing
 *   arrangement (Spanish conquest as papal-licensed subjugation) through the
 *   Spanish institutional lens—not through the Portuguese reading (same
 *   demarcation, opposite reading), not through the indigenous lens (no
 *   reading at all, only violent subjection), and not through a hypothetical
 *   post-colonial lens. The reading is the Spanish theological-legal reading
 *   of the bulls as constitutive grants for conquest.
 *
 * KEY AGENTS:
 *   - Spanish crown: agenda-setter, beneficiary; receives territorial title, labor rights, and tribute
 *   - Catholic Church and papacy: beneficiary, reinforces institutional legitimacy through continued grant authority
 *   - Indigenous populations west of line: victims, powerless, trapped; subject to conquest, conversion, labor extraction
 *   - Spanish colonial administrators and encomienda holders: agenda-setters, beneficiaries; enforce constraint, extract labor and land
 *   - Missionary orders: beneficiary, agenda-setter; enforce conversion mandate, manage religious legitimacy
 *   - Rival European powers: victims of exclusion; constrained from competing in western territories
 *   - Indigenous resistance movements: excluded, powerless; resist conquest militarily but lack institutional standing to contest papal legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.89).
domain_priors:suppression_score(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.91).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, extractiveness, 0.89).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse, 0.93).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, snare).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "Papal Grant License for Conquest and Indigenous Subjugation (Spanish Reading)").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "international law / colonial history / sovereignty").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__spanish_conquest_legitimation, '054bb129-7ef4-467e-8b48-2d47ba7d4ddd').
narrative_ontology:cs_kernel_codification('054bb129-7ef4-467e-8b48-2d47ba7d4ddd', fixed_text).
narrative_ontology:cs_authority_grounding('054bb129-7ef4-467e-8b48-2d47ba7d4ddd', extraction).
narrative_ontology:cs_interpretation_layer_present('054bb129-7ef4-467e-8b48-2d47ba7d4ddd').
narrative_ontology:cs_reading_relation('054bb129-7ef4-467e-8b48-2d47ba7d4ddd', tordesillas_demarcation_kernel__portuguese_exploration_legitimation, coexists_with).
narrative_ontology:cs_axiom('054bb129-7ef4-467e-8b48-2d47ba7d4ddd', foundational, papal_authority_grants_conquest_license).
narrative_ontology:cs_axiom_status(papal_authority_grants_conquest_license, holdable).
narrative_ontology:cs_axiom_grounding('054bb129-7ef4-467e-8b48-2d47ba7d4ddd', papal_authority_grants_conquest_license, deontological).
narrative_ontology:cs_axiom('054bb129-7ef4-467e-8b48-2d47ba7d4ddd', foundational, indigenous_peoples_lack_political_sovereignty).
narrative_ontology:cs_axiom_status(indigenous_peoples_lack_political_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('054bb129-7ef4-467e-8b48-2d47ba7d4ddd', indigenous_peoples_lack_political_sovereignty, empirically_contingent).
narrative_ontology:cs_axiom('054bb129-7ef4-467e-8b48-2d47ba7d4ddd', secondary, christian_conversion_obligation_justifies_coercion).
narrative_ontology:cs_axiom_status(christian_conversion_obligation_justifies_coercion, overridden).
narrative_ontology:cs_axiom_grounding('054bb129-7ef4-467e-8b48-2d47ba7d4ddd', christian_conversion_obligation_justifies_coercion, deontological).
narrative_ontology:cs_reference_frame('054bb129-7ef4-467e-8b48-2d47ba7d4ddd', papal_universal_temporal_authority_framework).
narrative_ontology:cs_drift_state('054bb129-7ef4-467e-8b48-2d47ba7d4ddd', enlightenment_sovereignty_challenge_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('054bb129-7ef4-467e-8b48-2d47ba7d4ddd', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_church_authority).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, rival_european_powers_excluded).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, papal_bureaucracy).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administrators).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, missionary_orders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Spanish monarchy receives papal authorization as a license to conquer, settle, and subjugate territories west of the demarcation line. The constraint operates through Spanish fleets, colonial administrators, and encomienda system enforcers. The crown justifies conquest through Christian conversion mandate and papal endorsement; the constraint persists because no higher terrestrial authority can override papal decree in the Spanish institutional framework.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown, agenda_setter,
    institutional, generational, arbitrage, global).

% The papacy grants the license and collects spiritual authority and territorial governance claims through the concordat structure. The church's legitimacy as a universal moral authority is reinforced by being the arbiter of legitimate territorial claims; this authority is maintained through performing the demarcation and endorsing Spanish actions as fulfilling Christian mission.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_church_authority, beneficiary,
    institutional, civilizational, arbitrage, universal).

% Indigenous peoples have no voice in the papal grant, no exit option (conquest claims their entire territorial domain), and no seat at any negotiation. Their situation is forcible incorporation into Spanish colonial administration, religious conversion under threat of violence, labor extraction through the encomienda system, and demographic collapse from disease and overwork. Their 'choice' is compliance or death.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line, payer,
    powerless, immediate, trapped, regional).

% Competing European powers (Portugal's own claim territory aside, France, England, the Dutch) are excluded from territories west of the line and must respect Spanish title backed by papal authority. Breaching the demarcation invokes conflict with Spain and religious illegitimacy. Their only exit is acceptance of the partition or costly military challenge to both Spain and papal legitimacy.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, rival_european_powers_excluded, payer,
    institutional, generational, constrained, global).

% The papal administration performs the demarcation authority role: issuing bulls, mediating disputes between Iberian powers, collecting spiritual revenue from the colonial territories, and maintaining institutional control over legitimacy definitions. The bureaucracy is enriched materially and institutionally by the ongoing grant-and-license system.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, papal_bureaucracy, beneficiary,
    institutional, generational, arbitrage, universal).

% Viceroy-level officials and encomienda holders extract labor, land, and tribute from indigenous peoples under the Spanish crown's delegated authority. Their power derives entirely from the papal license; without it, they have no legitimate claim to the territory or its inhabitants. They simultaneously enforce the constraint and benefit materially from it through land grants and labor monopolies.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administrators, agenda_setter,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administrators, beneficiary).

% Indigenous confederacies, empires, and organized peoples mount active military resistance: the Inca, the Aztec, regional coalitions. They are excluded from the legitimacy structure entirely—the papal grant never acknowledges their claims or consent. Their resistance persists because the constraint extracts so severely that acceptance means dissolution, but they lack the military technology and institutional unity to overcome Spanish conquest machinery.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_resistance_movements, excluded,
    organized, biographical, trapped, regional).

% Jesuit, Franciscan, and Dominican orders operate as spiritual enforcers and colonial extension. They justify the constraint theologically (conversion mandate, Christian obligation), manage labor through religious authority (confession, sacrament denial), and benefit institutionally (land grants, tithes, recruitment of indigenous clergy). Their consent and active enforcement make the constraint persistent.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, missionary_orders, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, missionary_orders, agenda_setter).

% The doctrine of papal temporal authority and the Christendom framework (the constraint operates via invocation of this doctrine, not its truth). The framework is reinforced and vindicated by Spanish use of the grant; questioning the grant requires questioning the framework itself—a meta-level move only available within theological debate, not to the powerless victims.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, theological_legitimacy_framework, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(tordesillas_demarcation_kernel__spanish_conquest_legitimation, theological_legitimacy_framework).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__spanish_conquest_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves potential conflict between Spanish and Portuguese colonial expansion claims in the Atlantic and newly encountered territories. Both crowns sought legitimacy through papal confirmation; the demarcation provides a single source of authority (the papacy) that partitions the world between them and prevents mutual naval warfare that would weaken both in Mediterranean and African conflicts.
% TRANSFER_FUNCTION: Transfers territorial sovereignty, labor rights, tribute obligations, and spiritual authority from indigenous populations to Spanish crown, Catholic Church, and Spanish colonial administrators. The constraint moves: indigenous labor into encomienda systems (forced), indigenous tribute to Spanish crown (forced), indigenous souls into Catholic conversion (coerced), resource extraction rights (metals, agricultural products, enslaved persons) to Spanish beneficiaries.
% ABSENT_VOICES: Indigenous populations whose lands and labor are the object of the constraint are completely absent from the negotiation and have no institutional standing to object. No indigenous representative participates in papal courts or Spanish crown deliberations. Excluded parties also include rival European powers (France, England, Dutch) whose territorial claims are foreclosed by the demarcation; they have standing only through later military challenge, not through the original negotiation.
% DISAPPEARANCE_RATIONALE: If the papal demarcation and grant vanished, Spanish territorial claims would lose their primary legitimating source in European law and Catholic doctrine. Without papal sanction, Spanish conquest becomes naked territorial grab without institutional legitimacy in European frameworks. Indigenous resistance would gain standing in European diplomatic discourse (the constraint's disappearance means indigenous polities have never consented). Rival European powers would immediately contest Spanish exclusive claims through privateering and eventually open colonial competition (France, England, Dutch beginning 1600s). The entire institutional arrangement of the Americas would reorganize around contested claims and indigenous agency rather than around a papal partition.
% FOUNDING_PROBLEM: The founding problem was the conflict between Spanish and Portuguese expansion claims in the Atlantic and newly discovered territories beyond Europe. Both powers had separate papal confirmations of prior rights; both claimed legitimacy. The coordination problem was how to partition the world between them without mutual naval warfare that would weaken both against Islamic powers and exhaustion of colonial resources.
% FOUNDING_PROBLEM_CORROBORATION: Spanish and Portuguese crowns jointly attest that the Treaty of Tordesillas solved the founding coordination problem and both benefited from demarcation clarity—at least initially (1494–1530). However, indigenous peoples and later rival European powers attested the problem was never their problem and the constraint persisted as a monopoly tool. Modern historical scholarship and indigenous historical accounts (outside the benefiting colonial powers) corroborate that the founding problem was dead by the 1600s when France, England, and Dutch powers broke the monopoly through actual conquest and settlement. The constraint thereafter operated to justify ongoing extraction rather than to prevent Spanish-Portuguese conflict.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__spanish_conquest_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__spanish_conquest_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is extremely high (0.89 at interval end) because the constraint transfers sovereignty, labor, land, and identity from indigenous peoples to Spanish beneficiaries with zero reciprocal obligation or protection. The constraint offers no coordination benefit to indigenous peoples—it is pure expropriation cloaked in theological language. Suppression is equally high (0.91) because indigenous exit options are nil: they cannot appeal to higher terrestrial authority, cannot negotiate the terms, cannot invoke competing legitimate frameworks (the Spanish institutional system accepts only papal authority and Christian doctrine). The constraint uses both external force (military conquest) and internalized authority (religious conversion, appeals to divine will) to suppress alternatives. Theater ratio rises over the interval (0.25 to 0.62) as the founding coordination problem (Spanish-Portuguese conflict) recedes and the constraint increasingly operates to justify ongoing extraction rather than to prevent war. The rising theater_ratio is Piton-adjacent signal: as rival European powers (French, English, Dutch) break the monopoly through actual conquest (1600s onward), Spanish legal doctrine and papal claims become increasingly performative—the constraint persists through institutional theater rather than through solving any live coordination problem. Accessibility collapse is near-total (0.93) because indigenous peoples have literally no alternative to accepting the constraint except non-existence; the alternatives available to some payers (rival Europeans can engage in privateering or colonial competition) do not apply to the primary victims (indigenous populations). The measurement series show extractiveness and suppression stabilizing at extremely high levels after 1620, consistent with a snare that has consolidated control and no longer needs to escalate coercion (the initial conquest phase required intensifying suppression; the mature colonial system maintains high suppression at stable levels).
 *
 * PERSPECTIVAL GAP:
 *   The constraint computes differently from each seat. From the Spanish agenda-setter seat: the arrangement is genuine coordination with Portugal, fulfilling Christian obligation to convert and govern justly. From the indigenous victim seat: the arrangement is naked conquest cloaked in language; no coordination occurred, only violent subjection. From rival European powers: the arrangement is a monopoly that excludes legitimate competition. From the papal seat: the arrangement is an exercise of universal temporal authority. The engine computes per-seat classifications from the structural data (power, exit options, beneficiary/victim positioning). The agenda-setter (institutional power, arbitrage exit, beneficiary role) and the victim (powerless, trapped exit, extraction target) occupy incompatible structural positions relative to the same constraint. The perspectival gap is not a matter of disagreement about facts—it is a matter of sitting on opposite sides of an extraction mechanism that operates through coercion and authority appeals that are binding on one seat (Spanish crown accepts papal authority) but illegitimate to the other (indigenous peoples never consented to papal authority and hold different cosmologies entirely).
 *
 * DIRECTIONALITY LOGIC:
 *   Spanish crown and Catholic Church sit at the beneficiary end (d ≈ 0.05): they collect sovereignty, labor, tribute, and spiritual authority without bearing the cost of the extraction. Spanish colonial administrators inherit beneficiary directionality through the crown's delegation (d ≈ 0.1). Indigenous populations sit at the full target end (d ≈ 0.95): they bear the extraction directly (labor, land, identity loss) with zero reciprocal gain and no exit. Trapped exit for indigenous peoples ensures directionality stays at the target end—even if they resist militarily (which they do), they cannot exit the territorial scope or appeal to a higher authority. Rival European powers sit slightly above neutral (d ≈ 0.55): they are constrained from accessing western territories (extraction), but they retain arbitrage options through privateering and eventually through breaking the monopoly militarily (hence not fully trapped). The directionality derivation is straightforward: structural victims at trapped exit + zero institutional alternatives = maximum target directionality; institutional beneficiaries with arbitrage options and higher authority backing = beneficiary directionality near zero.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is 'dead': the original coordination problem (preventing Spanish-Portuguese naval warfare over territory) is resolved by 1550 and stays resolved through the interval. The disappearance_verdict is 'world_rearranges' (if the constraint vanished, indigenous resistance would reorganize, rival powers would contest Spanish titles, the colonial system would need new legitimation). The mismatch (dead founding problem + world_rearranges verdict) triggers the mandatrophy flag. Mandatrophy is RESOLVED in this constraint story because the theater_ratio rise (0.25 to 0.62) and the persistence despite dead founding problem are accurately captured in the authored metrics and structural data. The constraint is now operating as pure extraction (encomienda, tribute, conversion-under-threat) justified through appeals to past papal authority. The canonical classifier will detect: founding_problem_status=dead AND disappearance_verdict=world_rearranges AND theater_ratio > 0.5 AND beneficiary/victim asymmetry AND requires_active_enforcement=true → Mandatrophy resolved via snare classification (the constraint is a snare because its persistence depends on coercion, not on any live coordination problem). The story does not need base_properties.mandatrophy_resolved=true because the structural data fully determines the state; the engine's computation will surface it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_authority_legitimacy,
    'Is papal temporal authority (authority to grant terrestrial sovereignty) genuine in the framework that makes this reading operative, or is it merely theatrical institutional claim?',
    'The resolution path requires analyzing whether the Spanish crown''s actual institutional practice treats papal authorization as binding or merely as convenient rhetorical cover. If papal authority is genuine, then the crown could not proceed without papal sanction; if theatrical, the crown proceeds with conquest regardless and invokes papal authority post hoc. Historical evidence shows Spain did seek papal authorization explicitly before conquest, suggesting genuine institutional binding—but the reading itself cannot be overturned by this evidence because the reading is about how the Spanish framework INTERPRETS the grant, not about whether the pope was actually sovereign.',
    'If papal authority is genuinely binding in the Spanish framework, the constraint''s persistence through institutional appeals to papal texts is well-grounded; if theatrical, the constraint is pure conquest dressed in legitimacy language. This affects how the engine should weight ''mandate extinction'' signals: if the authority is genuine, the founding problem (papal authorization for Spanish-Portuguese partition) remains live at the institutional level even if empirically dead; if theatrical, the theater_ratio signal (rising performance, stable extraction) is the key marker of mandatrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(papal_authority_legitimacy, conceptual, 'Whether papal authority is genuinely binding or institutionally rhetorical in the Spanish framework.').

omega_variable(
    indigenous_sovereignty_validity,
    'Did indigenous polities possess valid territorial sovereignty prior to Spanish conquest, and if so, does the papal grant override it through legitimate authority, or is the grant merely ex post facto justification for violent theft?',
    'This is a framework-dependent question: the Spanish reading assumes indigenous polities lack sovereignty (they are heathen, uncivilized, and thus available for Christian conquest). An indigenous reading would assert prior sovereignty and the grant as illegitimate usurpation. The resolution requires stepping outside the Spanish framework entirely—a move the Spanish reading cannot perform without ceasing to be the Spanish reading. Omega resolution would come from an indigenous-centered historical account that reconstructs pre-conquest polity organization and sovereignty claims, which by definition contradicts the Spanish reading''s premises.',
    'If indigenous sovereignty was valid prior, the Spanish reading is a false natural law (a constraint that looks like divine authorization but is actually constructed conquest). If indigenous sovereignty was null (the Spanish reading''s assumption), then the grant simply transfers unclaimed territory—a coordination story rather than a snare. The disagreement is about whether the foundational assumption (indigenous non-sovereignty) can be questioned within the Spanish reading or is axiomatic to it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_sovereignty_validity, conceptual, 'Whether indigenous territorial sovereignty is valid prior to the papal grant, making the grant either legitimate transfer or illegitimate usurpation.').

omega_variable(
    conversion_mandate_coercion_boundary,
    'Is the forced religious conversion of indigenous populations a legitimate exercise of Christian pastoral authority (the Spanish reading''s framing) or a coercive identity destruction (the indigenous experience and modern human rights frame)?',
    'This is a values-dependent (preference) question that cannot be resolved by further evidence. The Spanish reading holds that Christian conversion is obligatory for salvation and thus a benefit to indigenous peoples despite the coercion required. A human-rights reading holds that conversion under threat of violence is a violation of conscience. The boundary between ''righteous conversion duty'' and ''identity-destroying coercion'' is where the two readings truly part. Empirical evidence about outcomes (population collapse, cultural destruction, suicide rates during conversion) informs but cannot settle the values question.',
    'If conversion is legitimate pastoral authority, part of the extractiveness can be reframed as coordination cost (the price of spiritual salvation). If conversion is identity-destroying coercion, the entire missionary apparatus is pure extraction layered on top of conquest. This affects whether the constraint should be read as Tangled Rope (coordination + extraction) or as pure Snare (extraction masked by theology). The story authors it as Snare because the reading being instantiated (Spanish conquest legitimation) assumes coerced conversion is justified—not because conversion itself is analyzed as legitimate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conversion_mandate_coercion_boundary, preference, 'Whether forced religious conversion is legitimate Christian pastoral authority or illegitimate coercive identity destruction.').

omega_variable(
    suppression_internalized_vs_structural,
    'To what extent is indigenous compliance with Spanish rule internalized (adopted through religious conversion, administrative incorporation, identity fusion) versus structurally coerced (enforced by military violence, labor whipping, demographic collapse)?',
    'Post-exit observation: if indigenous polities that escape Spanish jurisdiction (those that maintain sovereignty or emigrate) show persistent compliance with Spanish doctrines and Spanish-aligned practices, suppression is internalized. If they actively reject Spanish authority and rebuild alternative institutions, suppression was structural. Historical evidence shows both: some indigenous populations adopt Spanish Catholicism and administrative frameworks (internalized), others resist continuously and maintain parallel institutions (structural suppression persists through force). The answer is likely ''mixed with high structural and variable internalized components''.',
    'High internalized suppression would reduce the psychological ceiling on exit (the constraint carries with escapees) but would also suggest the constraint is approaching coordination-like function (willing adoption of the framework). High structural suppression would reinforce the Snare classification and suggest mandatrophy risk (suppression machinery must be continuously maintained). The measured 0.91 suppression likely reflects both: military force + missionary conversion creating a mixed suppression profile. An omega here tracks the ambiguity in the measurement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalized_vs_structural, empirical, 'Whether measured suppression is internalized or structurally enforced or both.').

omega_variable(
    kernel_reading_boundary,
    'Is the contested reading located in the INTERPRETATION of the papal grant (what does authorization to conquer mean?), or in the VALIDITY of the grant itself (is a pope authorized to grant terrestrial sovereignty)?',
    'If the boundary is interpretive (the text is binding, the reading disputes what it says), then the two readings are genuine siblings of the same kernel. If the boundary is validity (one reading questions whether the pope has authority at all), then the readings are not siblings but competitors for different kernels. The historical record shows Spain and Portugal accepted papal validity (they sought papal mediation) but disagreed on interpretation (what did the grant authorize?)—suggesting the boundary is interpretive, not validity. However, later non-Iberian powers and indigenous histories question the validity entirely—suggesting a deeper kernel contest below the reading level.',
    'If the boundary is interpretive, the two sibling readings (Spanish conquest, Portuguese exploration) are authentic alternatives within the shared framework. If the boundary is validity, the readings are misnamed and the true kernel contest is whether papal authority is legitimate at all. This affects how the engine should relate the two constraints: as genuine siblings with coexists_with relation, or as steps in a deeper kernel-validity contest that would require a third constraint (papal_temporal_authority_validity, forecloses both Iberian readings).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the contested reading is located in interpretation of the grant or in validity of papal authority itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 1494, 1750).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t1494, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1494, 0.25).
narrative_ontology:measurement_basis(tord_tr_t1494, observed).
narrative_ontology:measurement(tord_tr_t1530, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1530, 0.35).
narrative_ontology:measurement_basis(tord_tr_t1530, observed).
narrative_ontology:measurement(tord_tr_t1570, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1570, 0.45).
narrative_ontology:measurement_basis(tord_tr_t1570, observed).
narrative_ontology:measurement(tord_tr_t1620, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1620, 0.55).
narrative_ontology:measurement_basis(tord_tr_t1620, observed).
narrative_ontology:measurement(tord_tr_t1680, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1680, 0.61).
narrative_ontology:measurement_basis(tord_tr_t1680, observed).
narrative_ontology:measurement(tord_tr_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1750, 0.62).
narrative_ontology:measurement_basis(tord_tr_t1750, observed).

% Extraction over time
narrative_ontology:measurement(tord_be_t1494, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1494, 0.72).
narrative_ontology:measurement_basis(tord_be_t1494, observed).
narrative_ontology:measurement(tord_be_t1530, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1530, 0.79).
narrative_ontology:measurement_basis(tord_be_t1530, observed).
narrative_ontology:measurement(tord_be_t1570, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1570, 0.84).
narrative_ontology:measurement_basis(tord_be_t1570, observed).
narrative_ontology:measurement(tord_be_t1620, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1620, 0.88).
narrative_ontology:measurement_basis(tord_be_t1620, observed).
narrative_ontology:measurement(tord_be_t1680, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1680, 0.89).
narrative_ontology:measurement_basis(tord_be_t1680, observed).
narrative_ontology:measurement(tord_be_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1750, 0.89).
narrative_ontology:measurement_basis(tord_be_t1750, observed).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1494, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1494, 0.75).
narrative_ontology:measurement_basis(tord_su_t1494, observed).
narrative_ontology:measurement(tord_su_t1530, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1530, 0.82).
narrative_ontology:measurement_basis(tord_su_t1530, observed).
narrative_ontology:measurement(tord_su_t1570, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1570, 0.87).
narrative_ontology:measurement_basis(tord_su_t1570, observed).
narrative_ontology:measurement(tord_su_t1620, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1620, 0.89).
narrative_ontology:measurement_basis(tord_su_t1620, observed).
narrative_ontology:measurement(tord_su_t1680, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1680, 0.91).
narrative_ontology:measurement_basis(tord_su_t1680, observed).
narrative_ontology:measurement(tord_su_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1750, 0.91).
narrative_ontology:measurement_basis(tord_su_t1750, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__spanish_conquest_legitimation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.15).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel__portuguese_exploration_legitimation).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomienda_system_colonial_labor).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, papal_temporal_authority_validity).

% DUAL FORMULATION NOTE:
% This constraint is one of two readings of the Tordesillas demarcation kernel. The sibling reading (portuguese_exploration_legitimation) instantiates the Portuguese interpretation of the same papal grant and treaty—namely, that the grant confirms prior exploration rights and excludes rivals east of the line, rather than licensing conquest west of the line. The two readings are coexistent (both parties held them simultaneously at the treaty signing) but structurally divergent in victim set, beneficiary structure, and extractiveness. Decomposition principle: the readings differ in what institutional consequences they claim from the grant (conquest vs. exploration protection), which changes the ε value, the victim set (indigenous peoples vs. rival European powers), and the persistence mechanism (conquest enforcement vs. diplomatic partition). Write each reading as a separate story; link them via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tordesillas_demarcation_kernel__spanish_conquest_legitimation, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
