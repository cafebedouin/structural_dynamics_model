% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__christianized_pacification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__christianized_pacification_reading, []).

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
 *   constraint_id: feud_obligation_kernel__christianized_pacification_reading
 *   human_readable: Feud Obligation Kernel (Christianized Pacification Reading)
 *   domain: legal/religious/political
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of a contested kernel: the
 *   feud-obligation kernel. The reading under examination here is the
 *   Christianized-pacification reading, which frames blood-feud obligations
 *   as violations of divine law that prohibit vengeance, locating legitimate
 *   violence authority with God and delegating it to ecclesiastical and royal
 *   institutions. This reading emerged in the early medieval Church (ca.
 *   800–900 CE) and intensified during the 10th–13th centuries as royal and
 *   ecclesiastical power consolidated. The constraint operates by claiming a
 *   monopoly on interpreting legitimate violence, declaring kinship-based
 *   revenge obligations sinful, and enforcing spiritual/temporal punishment
 *   on those who honor feud obligations. Other readings of the same kernel —
 *   the stateless-coordination reading (feuds as decentralized justice) and
 *   the extraction-cycle reading (feuds as destructive rent-taking) — frame
 *   the feud obligation itself differently. This story authoritatively
 *   describes the Christianized reading, not the others, and is linked to
 *   sibling stories via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Church hierarchy — institutional agenda-setter claiming interpretive monopoly on legitimate violence
 *   - Royal authority — institutional agenda-setter delegated violence authority, seeking territorial consolidation
 *   - Feud-obligated kingroups — moderate-power, identity-locked payers whose honor-based obligations are criminalized
 *   - Parish priests — local-level implementation agents with expanded authority over souls and discipline
 *   - Perpetrators and penitents — powerless victims bearing spiritual and material extraction costs
 *   - Competing kingroups — excluded from their traditional grievance outlets and forced into royal courts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, 0.82).
domain_priors:suppression_score(feud_obligation_kernel__christianized_pacification_reading, 0.91).
domain_priors:theater_ratio(feud_obligation_kernel__christianized_pacification_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__christianized_pacification_reading, snare).
narrative_ontology:human_readable(feud_obligation_kernel__christianized_pacification_reading, "Feud Obligation Kernel (Christianized Pacification Reading)").
narrative_ontology:topic_domain(feud_obligation_kernel__christianized_pacification_reading, "legal/religious/political").

domain_priors:requires_active_enforcement(feud_obligation_kernel__christianized_pacification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__christianized_pacification_reading, '62f72f65-9b85-4f9f-8121-e359ea304aac').
narrative_ontology:cs_kernel_codification('62f72f65-9b85-4f9f-8121-e359ea304aac', fixed_text).
narrative_ontology:cs_authority_grounding('62f72f65-9b85-4f9f-8121-e359ea304aac', lineage).
narrative_ontology:cs_interpretation_layer_present('62f72f65-9b85-4f9f-8121-e359ea304aac').
narrative_ontology:cs_reading_relation('62f72f65-9b85-4f9f-8121-e359ea304aac', feud_obligation_kernel__stateless_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('62f72f65-9b85-4f9f-8121-e359ea304aac', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_axiom('62f72f65-9b85-4f9f-8121-e359ea304aac', foundational, divine_monopoly_on_legitimate_violence).
narrative_ontology:cs_axiom_status(divine_monopoly_on_legitimate_violence, holdable).
narrative_ontology:cs_axiom_grounding('62f72f65-9b85-4f9f-8121-e359ea304aac', divine_monopoly_on_legitimate_violence, deontological).
narrative_ontology:cs_axiom('62f72f65-9b85-4f9f-8121-e359ea304aac', foundational, kinship_obligation_incompatible_with_divine_law).
narrative_ontology:cs_axiom_status(kinship_obligation_incompatible_with_divine_law, holdable).
narrative_ontology:cs_axiom_grounding('62f72f65-9b85-4f9f-8121-e359ea304aac', kinship_obligation_incompatible_with_divine_law, deontological).
narrative_ontology:cs_axiom('62f72f65-9b85-4f9f-8121-e359ea304aac', secondary, ecclesiastical_interpretive_authority_over_legitimacy).
narrative_ontology:cs_axiom_status(ecclesiastical_interpretive_authority_over_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('62f72f65-9b85-4f9f-8121-e359ea304aac', ecclesiastical_interpretive_authority_over_legitimacy, conventional).
narrative_ontology:cs_reference_frame('62f72f65-9b85-4f9f-8121-e359ea304aac', divine_law_as_sole_legitimate_violence_authority).
narrative_ontology:cs_drift_state('62f72f65-9b85-4f9f-8121-e359ea304aac', consolidation_era_1300, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('62f72f65-9b85-4f9f-8121-e359ea304aac', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, church_hierarchy).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, royal_authority).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, feud_obligated_kingroups).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, perpetrators_of_violence).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, penitents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, parish_priests).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__christianized_pacification_reading, divine_monopoly_on_legitimate_violence).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_interpretive_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and enforces doctrine that blood-feud obligations violate divine law. Administers penitential discipline and spiritual remedies. Expands jurisdictional reach by bringing feud disputes into ecclesiastical courts and framing resolution as spiritual reconciliation rather than compensation or blood-price. Collects authority and expanded institutional reach through the monopoly on interpreting what constitutes legitimate violence.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, church_hierarchy, agenda_setter,
    institutional, civilizational, analytical, continental).

% Allies with Church doctrine to criminalize feud obligation enforcement. Benefits from reduction in autonomous kingroup violence that had challenged territorial consolidation. Delegates much of the enforcement machinery to ecclesiastical institutions while retaining ultimate coercive power. Frames feud suppression as bringing order and Christian civilization.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, royal_authority, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, royal_authority, beneficiary).

% Face spiritual condemnation for honoring kinship obligations to pursue vengeance. Their identity is fused with feud participation — honor, lineage loyalty, and revenge obligation are constitutive of kingroup standing. Doctrine declares their core obligation sinful and demands penitential submission. They face suppression through spiritual threat (damnation, excommunication) and coercive enforcement (royal and ecclesiastical). Exit would require abandoning kingroup identity or severing blood ties.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, feud_obligated_kingroups, payer,
    moderate, biographical, identity_locked, regional).

% Those who have committed killing or wounding within feud cycles face dual condemnation: from the Church (mortal sin requiring extreme penitential remedy, potentially including public confession, pilgrimage, monastery refuge, or death-bed absolution) and from royal justice (criminal prosecution, execution, or forced compensation). They are trapped between kingroup revenge obligation (which would demand reciprocal killing) and ecclesiastical/royal prohibition (which treats such action as violation of divine law). Escape routes are extremely constrained.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, perpetrators_of_violence, payer,
    powerless, biographical, trapped, local).

% Those who accept the Church's doctrine and seek absolution undergo prescribed penances: public flagellation, monastery service, crusade participation, pilgrimages, or restitution. The penitential system extracts labor, wealth, and mobility. Church administers the terms and decides when penance is complete. The penitent's status shifts from kingroup member to penitent, creating identity confusion and social isolation.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, penitents, payer,
    powerless, biographical, constrained, local).

% Groups whose members have been killed or injured in feud cycles have traditional standing to pursue vengeance or demand compensation. Ecclesiastical doctrine removes their legitimate grievance outlet by declaring revenge sinful. Royal courts may offer compensation, but only on terms set by royal authority. They are excluded from the traditional self-enforcing justice mechanism that feud provided, with no equivalent mechanism substituted except submission to royal/ecclesiastical institutions.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, competing_kingroups, excluded,
    moderate, biographical, constrained, regional).

% Implement ecclesiastical doctrine at the local level: hear confessions, assign penances, administer spiritual condemnation to feud participants, mediate disputes. Their authority over souls and salvation becomes the enforcement mechanism. They benefit through expanded institutional authority and control over local populations, but are also identity-fused with the doctrine — their role depends on the Church's monopoly on spiritual legitimacy.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, parish_priests, agenda_setter,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, parish_priests, beneficiary).

% Regional lords occupy an unstable position: they may ally with Church/royal authority to suppress feuds among lower-status kingroups while practicing revenge and territorial conquest themselves. Some resist ecclesiastical doctrine as threat to their own honor-based authority; others adopt it selectively. Their power position allows negotiation with both Church and royal authority over where doctrine is enforced.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, secular_nobles, observer,
    powerful, generational, arbitrage, regional).

% Those killed or injured in feud cycles exist in ambiguous position: the Church's doctrine may console them spiritually (murderer faces damnation) but offers no material compensation or restitution. They are excluded from traditional feud-resolution processes because those processes are now criminalized. Royal courts may offer compensation, but extraction of feud obligation means competing kingroups cannot pursue their grievance through traditional channels.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, victims_of_feud_violence, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__christianized_pacification_reading, church_hierarchy).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__christianized_pacification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reading proposes a coordination solution to endemic feud cycles: shift violence-legitimacy authority from kingroup to centralized ecclesiastical and royal institutions, removing the self-enforcing cycle of obligation and counter-violence. The new authority claims monopoly on determining what violence is legitimate (punishment, crusade, justice) and delegates this authority from God through Church and Crown.
% TRANSFER_FUNCTION: Transfers spiritual authority and jurisdictional reach from kingroup elders to Church hierarchy. Transfers enforcement power from feud participants to ecclesiastical and royal courts. Transfers wealth through penitential payments, restitution, and confiscations. Transfers legitimacy claims: what was honorable (revenge obligation) becomes sinful; what was dishonorable (submission to external judgment) becomes salvific.
% ABSENT_VOICES: Feud-obligated kingroups are systematically excluded from defining what constitutes legitimate grievance and appropriate response. Perpetrators of feud violence cannot speak their honor-based justification without facing spiritual condemnation. Competing kingroups whose members have been harmed are excluded from pursuing their traditional remedy and offered only submission to royal judgment in its place. Secular nobles who see feud-suppression as threat to their own hierarchical authority are present but must speak guardedly against Church doctrine.
% DISAPPEARANCE_RATIONALE: If the Christianized doctrine that feud obligation violates divine law were to disappear — if ecclesiastical authority no longer claimed monopoly on legitimate violence and kingroups were free to pursue blood-price or revenge without spiritual peril — the institutional arrangement would collapse entirely. Royal courts would lose their ecclesiastical backing for feud suppression. Parish priests would lose their authority to condemn feud participants spiritually. Kingroups would revert to autonomous justice mechanisms. The territorial consolidation achieved through feud-suppression would be at risk of dissolution. Penitential discipline would cease, removing a major extraction mechanism. The constraint is fundamental to the institutional reorganization it claims to facilitate.
% FOUNDING_PROBLEM: Blood-feud obligations created destructive cycles of reciprocal violence that prevented stable territorial authority, made contract enforcement and commerce difficult, and killed productive members of society. Early Church and developing monarchies sought to centralize legitimate violence authority and eliminate autonomous kingroup justice.
% FOUNDING_PROBLEM_CORROBORATION: Church documents and royal edicts from the 9th–13th centuries attest the founding problem as live and dire. However, historians and anthropological observers note that feuds also served as decentralized justice mechanism providing deterrence and compensation in absence of centralized enforcement. The 'founding problem' framing emphasizes harm from feuds while backgrounding their coordination function. Secular nobles and feud-obligated kingroups (where recorded voices survive) attest that the problem was less dire than ecclesiastical/royal narratives claimed, and that solutions came at severe cost to kingroup autonomy and honor. The founding problem status is genuinely contested between the reading's beneficiaries (Church/Crown) and those bearing its costs.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__christianized_pacification_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__christianized_pacification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__christianized_pacification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feud_obligation_kernel__christianized_pacification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__christianized_pacification_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.82 by interval end because the reading claims authority to redefine what is sinful and legitimate — a massive appropriation of legitimacy from kingroup elders to ecclesiastical and royal institutions. The actual feuds-as-violence continue (the founding problem persists), but the reading declares that those continuing feuds are now automatically sinful, extracting from feud participants the capacity to act honorably. Suppression is very high (0.91) because enforcing this doctrine requires sustained spiritual threat (damnation, excommunication), coercive institutions (royal courts, ecclesiastical courts, monasteries), and identification of non-compliance. Theater is substantial (0.58 at interval end, rising from 0.28) because much of the enforcement apparatus is performative: public penitential rituals, processions, confessions, monastic refuge — visible signs of authority that work partly by spectacle. Accessibility collapse is moderate (0.72) because alternatives remain available to feud participants (secret vengeance, secular noble protection, migration) but are increasingly costly. Resistance is high (0.74) because kingroups actively resist the reframing of their obligations as sinful — resistance takes forms from non-compliance to outright rejection of ecclesiastical authority. The measurement series documents the intensification of the constraint's operation over 500 years: extractiveness rises as Church doctrine hardens and enforcement infrastructure matures; suppression rises as royal and ecclesiastical courts expand; theater rises as the penitential system becomes more elaborate.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute different types for different seats. From the Church/Crown seat: this is rope or tangled-rope (real coordination function — eliminating destructive feuds — bundled with extraction — appropriating legitimacy). From the feud-obligated kingroup seats: this is snare (the coordination story is cover; persistence depends on coercion and suppressing exits; there are identifiable victims). The gap exists in the structure: the constraint genuinely solves a coordination problem for the territorial consolidation project, AND genuinely extracts legitimacy and capacity from kingroups. Both readings are structurally true — the divergence is the point.
 *
 * DIRECTIONALITY LOGIC:
 *   Church and royal authority sit at the beneficiary end (d near 0.0 for institutional seats): they appropriate interpretive authority, expand jurisdictional reach, and consolidate power. They face minimal exit costs — the doctrine is their creation and they control its evolution. Feud-obligated kingroups sit at the target end (d near 1.0): they are forced to abandon honor-based obligations, face spiritual peril for compliance failure, and have exit routes closed (identity-locked: kingroup membership depends on honoring feud obligations, so exiting the obligation means exiting the kingroup). Perpetrators and penitents sit at the full-target end because they face the direct spiritual and coercive extraction: they are trapped between obligation and condemnation. Competing kingroups are targeted (excluded from justice mechanisms, forced into royal courts) with moderate-high d. Parish priests occupy an unstable middle: they benefit from expanded authority (moderate beneficiary role) but are also identity-locked to the doctrine they implement (if the doctrine failed, their local authority collapsed). The per-seat classification should diverge substantially: from the Church's institutional seat this looks like successful pacification (low χ, net benefit from consolidation); from the feud-participant seats this looks like pure extraction of legitimacy and capacity to act (high χ, severe cost).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — endemic blood feuds — is contestable. Church documents and royal edicts claim the problem is still live and dire at interval end (1300 CE). However, alternative evidence (secular noble records, kingroup genealogies where they survive, historical analysis) suggests the founding problem had substantially resolved by 1200 and that afterward the constraint persisted partly by inertia and theatrical maintenance of the pacification narrative. The authored theater_ratio rising from 0.28 to 0.58 hints at this: as feuds actually declined (founding problem faded), the penitential system became more elaborate and performative (theater increased). If the founding problem died while the constraint persisted, this is a mandatrophy candidate. However, the founding_problem_status is authored as contested (not dead), reflecting genuine historical disagreement about whether feuds were truly vanquished or merely driven underground and integrated into noble political competition. This ambiguity belongs in an omega, not resolved in the base classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_monopoly_reading_vs_political_consolidation,
    'Is the Christianized reading grounded in authentic theological conviction that vengeance violates divine law, or is it instrumentally constructed to justify institutional consolidation of violence authority?',
    'Textual and institutional history: examine whether the doctrine emerges from internal theological development (e.g., Augustinian just-war theory, Old Testament exegesis) or is retrofitted to justify political centralizations. Compare doctrine''s trajectory in regions with strong Church authority versus weak authority.',
    'If theological, the constraint''s ε is lower (the doctrine constrains even its beneficiaries); if instrumental, ε is higher (pure extraction of legitimacy dressed as theology). The axiom grounding_type shifts from deontological to conventional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_monopoly_reading_vs_political_consolidation, empirical, 'Whether the reading is theologically grounded or instrumentally constructed for political consolidation.').

omega_variable(
    identity_lock_mechanism_structural_vs_internalized,
    'Is the feud-obligated kingroup''s exit constraint (identity_locked) primarily structural (kingroup members are materially dependent on the group and cannot leave without economic catastrophe) or internalized (members believe they deserve the constraint and cannot imagine honorable exit)?',
    'Post-suppression trajectory: in regions where feud suppression succeeded earliest and most completely (e.g., Norman England, Capetian France), do kingroup members who abandon feud obligations show psychological distress (internalized suppression) or relief (structural exit from coercion)? Multi-generational outcomes tracking.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — it carries with it after suppression fades. If structural, suppression drops sharply when enforcement weakens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_structural_vs_internalized, empirical, 'Whether identity-lock is structural dependence or internalized psychological fusion.').

omega_variable(
    founding_problem_live_vs_dead,
    'At the interval end (1300 CE), is the founding problem (destructive feud cycles) still live as described in ecclesiastical sources, or has it substantially faded and the constraint persists by inertia and theater?',
    'Quantitative evidence: count of feud-related deaths and compensation disputes in primary sources (charters, court records, chronicles) over the interval. Compare regions with strong ecclesiastical suppression to regions with weaker enforcement. Distinguish whether declining feuds correlates with suppression enforcement or with other factors (population growth, territorial consolidation, monetary economy development).',
    'If the problem is dead while the constraint persists with rising theater, this is a strong mandatrophy signal — the constraint is maintained by institutional inertia and theatrical performance, not by solving an actual problem. Reclassification from snare → piton likely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_live_vs_dead, empirical, 'Whether the founding problem persists or has been solved while the constraint remains.').

omega_variable(
    coexistence_of_readings_empirical_bounds,
    'How much does the coexistence of this reading (Christianized-pacification) with the stateless-coordination reading depend on geographic and temporal variation (different regions hold different readings) versus genuine within-same-group cognitive coexistence (the same kingroup/noble simultaneously holds both readings)?',
    'Institutional analysis: which readings are held by which actors in which regions and periods? Textual analysis of same-author documents: do Church writers and noble writers evidence cognitive coexistence (both praising feud suppression AND kinship loyalty) or are they in different discursive zones?',
    'Pure geographic coexistence (stateless-coordination in kingroups, Christianized-pacification in Church) is simpler and suggests the readings genuinely foreclose each other at the individual level. Cognitive coexistence suggests more complex institutional dynamics where both readings remain simultaneously live.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coexistence_of_readings_empirical_bounds, empirical, 'Whether sibling readings coexist across actors or within them.').

omega_variable(
    beneficiary_capture_of_doctrine_interpretation,
    'Does the Church''s monopoly on interpreting divine law regarding legitimate violence create feedback where the Church''s institutional interests (expanded jurisdiction, wealth collection through penitential payments) increasingly shape the doctrine itself, leading to doctrine-creep and expanded extraction?',
    'Doctrinal analysis: track how Church teachings on feud, penance, and legitimate violence change over the interval. Compare teachings from periods when the Church faced resistance to teachings from periods of consolidated authority. Look for expansion of penance burdens, penitential fees, and conditions for absolution over time.',
    'If beneficiary capture is occurring, the rising theater_ratio and rising extractiveness over time are explained by institutional drift rather than problem persistence. The constraint degrades from snare (responding to feud problem) toward snare-with-theater (maintaining extraction machinery).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_doctrine_interpretation, empirical, 'Whether the Church''s institutional interests shape doctrine evolution toward expanded extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__christianized_pacification_reading, 800, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t800, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 800, 0.28).
narrative_ontology:measurement_basis(feud_tr_t800, projected).
narrative_ontology:measurement(feud_tr_t950, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 950, 0.38).
narrative_ontology:measurement_basis(feud_tr_t950, observed).
narrative_ontology:measurement(feud_tr_t1100, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1100, 0.48).
narrative_ontology:measurement_basis(feud_tr_t1100, observed).
narrative_ontology:measurement(feud_tr_t1200, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1200, 0.54).
narrative_ontology:measurement_basis(feud_tr_t1200, observed).
narrative_ontology:measurement(feud_tr_t1300, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1300, 0.58).
narrative_ontology:measurement_basis(feud_tr_t1300, observed).

% Extraction over time
narrative_ontology:measurement(feud_be_t800, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 800, 0.35).
narrative_ontology:measurement_basis(feud_be_t800, projected).
narrative_ontology:measurement(feud_be_t950, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 950, 0.52).
narrative_ontology:measurement_basis(feud_be_t950, observed).
narrative_ontology:measurement(feud_be_t1100, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1100, 0.68).
narrative_ontology:measurement_basis(feud_be_t1100, observed).
narrative_ontology:measurement(feud_be_t1200, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1200, 0.78).
narrative_ontology:measurement_basis(feud_be_t1200, observed).
narrative_ontology:measurement(feud_be_t1300, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1300, 0.82).
narrative_ontology:measurement_basis(feud_be_t1300, observed).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t800, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 800, 0.55).
narrative_ontology:measurement_basis(feud_su_t800, projected).
narrative_ontology:measurement(feud_su_t950, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 950, 0.68).
narrative_ontology:measurement_basis(feud_su_t950, observed).
narrative_ontology:measurement(feud_su_t1100, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1100, 0.78).
narrative_ontology:measurement_basis(feud_su_t1100, observed).
narrative_ontology:measurement(feud_su_t1200, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1200, 0.86).
narrative_ontology:measurement_basis(feud_su_t1200, observed).
narrative_ontology:measurement(feud_su_t1300, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1300, 0.91).
narrative_ontology:measurement_basis(feud_su_t1300, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__christianized_pacification_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__christianized_pacification_reading, 0.18).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__extraction_cycle_reading).

% DUAL FORMULATION NOTE:
% The feud-obligation kernel decomposes into three structurally distinct constraint stories corresponding to three readings of the same practices (blood-feud vengeance obligations). This story (Christianized-pacification reading) frames feuds as violations of divine law. The stateless-coordination reading frames them as decentralized justice mechanisms. The extraction-cycle reading frames them as destructive rent-extraction cycles. All three readings claim the same referent (the practices themselves) but author different ε values and different beneficiary/victim sets because they assess the same practices through different frameworks. The three constraints are linked here to signal their genealogical relationship. Boundaries: this story does NOT describe the feud practices themselves as a neutral phenomenon; it describes them AS SEEN by the Christianized reading. The stateless-coordination and extraction-cycle readings are separate constraint files with their own ε, their own stakeholders, their own six-questions answers. No single file tries to contain the contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feud_obligation_kernel__christianized_pacification_reading, institutional, 0.05).
constraint_indexing:directionality_override(feud_obligation_kernel__christianized_pacification_reading, moderate, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
