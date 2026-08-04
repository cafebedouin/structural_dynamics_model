% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__contraction_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor-Violence Legitimacy Contraction: Dueling as Conceptually Illegitimate
 *   domain: social/legal/cultural
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested kernel 'honor
 *   and violence.' The contraction reading claims that dueling became
 *   structurally unthinkable as honor itself was conceptually redefined.
 *   Where the drop reading says dueling remained conceptually legitimate but
 *   became practically rare, and the composite reading says both mechanisms
 *   operated simultaneously, the contraction reading asserts that the very
 *   definition of what-counts-as-honor shifted: violence-through-combat was
 *   expelled from the legitimate set. The state, church, and professional
 *   intellectual classes coordinated this redefinition through law, moral
 *   teaching, and cultural patronage. Practitioners of honor culture
 *   (military aristocracy, provincial gentry) were forced to exit a practice
 *   that had constituted their identity. The constraint is CLAIMED as
 *   tangled_rope because it performs real coordination (state monopoly on
 *   violence) while extracting from those whose entire status system it
 *   negates; it MEASURES as substantially extractive (0.68), actively
 *   suppressed (0.72), and moderately theatrical (0.41). The claim-metric
 *   divergence reflects the seat divergence: from state and professional
 *   perspective, the constraint is coordination that enabled civilization;
 *   from practitioner perspective, it is coercive redefinition that
 *   constitutes identity destruction.
 *
 * KEY AGENTS:
 *   - State legal apparatus: agenda-setter, benefits from violence monopoly consolidation
 *   - Military aristocracy: payer, identity-locked, faces legal liability and moral redefinition
 *   - Provincial gentry: payer, identity-locked, loses local authority mechanism
 *   - Urban professional classes: beneficiary, gains institutional standing from law-and-civility narrative
 *   - Institutional church: beneficiary-agenda-setter, increases moral authority over honor narratives
 *   - Intellectual reform coalition: agenda-setter-beneficiary, produces and gains standing from the redefinition
 *   - Women in honor culture: payer-beneficiary, loses violent death risk but reified into sexual property
 *   - Excluded alternative honor framings: suppressed rival legitimacy systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.68).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.72).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor-Violence Legitimacy Contraction: Dueling as Conceptually Illegitimate").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "social/legal/cultural").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, '7c14f827-4703-4ec6-9f7b-1fcd6ff0a85a').
narrative_ontology:cs_kernel_codification('7c14f827-4703-4ec6-9f7b-1fcd6ff0a85a', formalized).
narrative_ontology:cs_authority_grounding('7c14f827-4703-4ec6-9f7b-1fcd6ff0a85a', extraction).
narrative_ontology:cs_interpretation_layer_present('7c14f827-4703-4ec6-9f7b-1fcd6ff0a85a').
narrative_ontology:cs_reading_relation('7c14f827-4703-4ec6-9f7b-1fcd6ff0a85a', honor_violence_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c14f827-4703-4ec6-9f7b-1fcd6ff0a85a', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('7c14f827-4703-4ec6-9f7b-1fcd6ff0a85a', foundational, honor_and_violence_contradictory).
narrative_ontology:cs_axiom_status(honor_and_violence_contradictory, holdable).
narrative_ontology:cs_axiom_grounding('7c14f827-4703-4ec6-9f7b-1fcd6ff0a85a', honor_and_violence_contradictory, deontological).
narrative_ontology:cs_axiom('7c14f827-4703-4ec6-9f7b-1fcd6ff0a85a', secondary, civility_requires_renunciation_of_combat).
narrative_ontology:cs_axiom_status(civility_requires_renunciation_of_combat, holdable).
narrative_ontology:cs_axiom_grounding('7c14f827-4703-4ec6-9f7b-1fcd6ff0a85a', civility_requires_renunciation_of_combat, instrumental).
narrative_ontology:cs_reference_frame('7c14f827-4703-4ec6-9f7b-1fcd6ff0a85a', honor_via_martial_valor).
narrative_ontology:cs_drift_state('7c14f827-4703-4ec6-9f7b-1fcd6ff0a85a', post_enlightenment_institutional_consolidation, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('7c14f827-4703-4ec6-9f7b-1fcd6ff0a85a', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, state_legal_apparatus).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, urban_professional_classes).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, institutional_church).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, military_aristocracy).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, provincial_gentry).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, honor_culture_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, women_in_honor_culture).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, intellectual_reform_coalition).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, women_in_honor_culture).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, state_monopoly_on_legitimate_violence).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, civility_as_marker_of_civilization).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, rational_law_supersedes_custom).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the legal and moral framework that dueling is murder/assault and honor satisfaction requires law, not combat. Prosecutes duelists, excludes them from office, denies legal recovery for honor injuries. Directly consolidates state monopoly on legitimate violence through this redefinition.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Practitioners of honor-via-combat. Face legal prosecution, career exclusion, and social stigma. Identity fundamentally fused with martial honor — to renounce dueling is to renounce selfhood. Locked into choosing between practicing illegally (death or exile) or accepting redefinition that negates their status system.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, military_aristocracy, payer,
    powerful, generational, identity_locked, national).

% Rural honor practitioners dependent on combat reputation for local standing and marriage negotiations. Face same legal and moral suppression with fewer alternative channels. Identity deeply bound to honor-by-combat; exit means losing all status mechanisms simultaneously.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, provincial_gentry, payer,
    moderate, biographical, identity_locked, regional).

% Lawyers, merchants, civil servants, physicians, intellectuals. Gain institutional authority from the law-and-civility narrative that defines them as civilized. Their honor redefined as professional reputation and intellectual achievement — channels that actually benefit when dueling is suppressed. Can exit the old honor system freely.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, urban_professional_classes, beneficiary,
    organized, biographical, mobile, national).

% Religious authorities condemn dueling as sin and murder, increasing spiritual authority over honor narratives. The redefinition of honor as compatible with Christian renunciation strengthens the church's claim to define civilization and morality. Excommunication and confession denial enforce the constraint.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, institutional_church, beneficiary,
    institutional, generational, analytical, national).

% Mothers and wives bear violent death and disability of duelists and legal punishment. Also bear costs of honor culture itself (marriage as status negotiation, sexual honor as asset). Redefinition reduces violent death but reifies women into honor-via-chastity. Mixed extraction: less violence but deeper property status.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, women_in_honor_culture, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__contraction_reading, women_in_honor_culture, beneficiary).

% Writers, philosophers, salon hosts, reformers who produce and circulate the reframing. Gain standing through offices, patronage, and institutional positioning as authorities on civilization and morality. Jointly author the contraction narrative through satire, moral argument, and legal reform advocacy.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, intellectual_reform_coalition, agenda_setter,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__contraction_reading, intellectual_reform_coalition, beneficiary).

% Rival definitions of honor centered on martial valor or lineage status. Not formally defeated but suppressed through legal enforcement and institutional exclusion. State and church could not simultaneously endorse both combat-honor and law-honor—the structural foreclosure is what the contraction reading claims.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, excluded_alternative_honor_framings, excluded,
    powerful, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(honor_violence_legitimacy__contraction_reading, excluded_alternative_honor_framings).

% Examines the constraint structure: how the redefinition operated, what structural work it performed, what was suppressed under the cover of coordination, how different seats experienced the same mechanism inversely.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__contraction_reading, state_legal_apparatus).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinated the transition from decentralized private violence (as legitimate honor response) to state-centered law as the sole legitimate channel for status maintenance and dispute resolution. Consolidates enforcement power and reduces open warfare between aristocratic factions.
% TRANSFER_FUNCTION: Transfers the definition of honor and legitimacy from dispersed practitioner communities to centralized institutions (state, church, professional bodies). Also transfers violence-monopoly benefit from private combatants to state apparatus. Moves legitimacy narratives from martial valor to civility, professionalism, law-compliance. Military aristocracy and provincial gentry lose their primary status mechanism; urban professionals and state actors gain institutional standing.
% ABSENT_VOICES: Rural and provincial practitioners unable to reach courts or intellectual salons; soldiers and warriors whose professional identity centered on combat prowess; practitioners of honor culture outside Western/European institutional contexts; populations where honor-via-combat persisted outside state jurisdiction; practitioners who continued dueling underground and left no official testimony.
% DISAPPEARANCE_RATIONALE: If the redefinition disappeared and honor-via-combat reverted to legitimate status claim, the entire architecture of state monopoly on violence would collapse. Aristocratic factions would re-arm; professional classes would lose moral superiority claims; institutional authority would require re-anchoring. The constraint's disappearance would catastrophically destabilize the modern state system's self-understanding.
% FOUNDING_PROBLEM: Private violence among honor-bound elites threatened state consolidation. Dueling drained military personnel, destabilized political hierarchies, and undermined state claims to monopoly on legitimate violence. The problem was existential to state formation and required either suppression or redefinition.
% FOUNDING_PROBLEM_CORROBORATION: State reformers and beneficiary-side historians attest the founding problem was live and required resolution. Military practitioners attest that honor satisfaction required combat and its legal suppression forced identity exile. Contemporary historians document both readings: states genuinely faced destabilization AND used that problem to suppress rival authority systems. Sources outside both benefiting and practicing parties (neutral historians, sociologists, anthropologists) document the redefinition as politically motivated institutional consolidation, not natural moral progress.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 to 0.68 over the interval because initial cultural competition left alternatives available (low extraction), while progressive legal enforcement, institutional exclusion, and moral reframing narrows the set of defensible honor claims (rising extraction). By interval end, dueling is not merely illegal but conceptually illegitimate—exit has been engineered into internalization. Suppression rises from 0.44 to 0.72 because the constraint's holding power shifts from legal penalties (early) to institutional gatekeeping and identity fusion (late)—the coercive mechanism is actively maintained by schools, salons, churches, and courts. Theater rises from 0.18 to 0.41 because the state's claim to represent civilization and the church's claim to represent morality become themselves performative: they require continuous reassertion that law-bound professionals ARE more civilized than martial practitioners. Accessibility collapse rises across all levels because alternatives to law-and-civility are progressively erased from the legitimate conceptual space. Resistance falls from high (0.72 at individual, 0.68 at organizational) to low (0.31, 0.25) because individual practitioners lose institutional power and class-level cohesion breaks as professional classes defect to the new honor system. Suppression rises at all levels because enforcement is multidirectional: state law enforcement against individual duelists, institutional exclusion from office and church, professional-class moral condemnation, and women-in-honor-culture role redefinition all operate simultaneously.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence is maximal between state-and-church (beneficiary-side) and military-aristocracy-and-gentry (target-side). From state/church perspective, the constraint is genuine coordination solving a real problem (preventing aristocratic internal warfare that destabilized state formation) while vindicated propositions (state monopoly, civility, rational law) represent genuine improvements. From practitioner perspective, the same constraint is coercive redefinition that erased their status system, forced them into identity exile (continued dueling underground = death or dishonor), and transferred their authority to institutional actors who had never risked honor-by-combat. The engine computes this divergence from the structural data: agenda-setter vs. payer roles, beneficiary vs. victim declarations, and identity-locked vs. mobile exit produce opposite directionality values and thus opposite type classifications at each seat. The analytical observer seat (included explicitly here) sees the constraint as neither coordination nor extraction but as a structurally enforced shift in what-counts-as-legitimate, a redefinition mechanism that beneficiaries and payers experience inversely.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality assignments flow from beneficiary/victim declarations and exit options. State legal apparatus (institutional, analytical exit) is near beneficiary end (d~0.15): it collects the violence monopoly and enforcement authority, constrained only by its own internal logic. Military aristocracy (powerful power atom, identity-locked exit) is at target end (d~0.85): they lose their primary legitimacy mechanism and cannot exit—the identity fusion locks them in. Provincial gentry (moderate power, identity-locked) are also at target end (d~0.8): they have no institutional alternative channels and depend on honor-for-standing in ways urban professionals do not. Urban professionals (organized power, mobile exit) sit near beneficiary (d~0.2): they benefit from the institutional standing the law-and-civility narrative grants them, and can exit by entering the alternative honor system whenever they choose (though social cost is high). Church (institutional, analytical) is beneficiary-side (d~0.1): it gains moral authority. Women are complex: they are payers in the violence domain (bearing the costs of dueling deaths) and identity-locked in marriage-and-status domain, but beneficiaries in the reduction of violent death and potential institutional participation. Measured d~0.55 reflects the ambiguity.
 *
 * MANDATROPHY ANALYSIS:
 *   The contraction reading prevents confusion between coordination and extraction at the definitional level. A naive reading might treat the constraint as pure rope: states coordinate on law-based legitimate violence, everyone benefits from reduced aristocratic warfare. But structural analysis shows this is tangled_rope exactly because the 'coordination function' (state monopoly) is inseparable from the 'extraction function' (transferring authority from practitioners to institutions): state actors could have merely criminalized dueling while leaving the honor-based legitimacy claim alive (drop reading), but instead they attacked honor itself. The mandatrophy is resolved by noting that founding_problem (aristocratic violence destabilizing state) is contested as still-live: practitioners claim honor is still necessary for personal standing (status=live from practitioner read), while state claims law replaced honor (status=dead from state read). The disappearance_verdict (world_rearranges) indicates the constraint is not natural law but constructed arrangement. The mismatch (live founding problem + world_rearranges + theater_ratio rising) signals that the constraint is performing extraction under the cover of coordination, which is exactly what tangled_rope should detect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_mechanism,
    'Is the measured suppression of dueling practitioners structural (legal barriers, career exclusion, institutional barriers to status) or internalized (the practitioners came to believe dueling was immoral/illegitimate)?',
    'Post-suppression historical analysis: did practitioners who escaped legal jurisdiction or retired to safe exile regions voluntarily renounce dueling, or did they continue practicing where legal suppression was absent? Did acceptance of the redefinition precede or follow legal enforcement?',
    'If suppression is primarily structural, the constraint''s holding power depends on enforcement infrastructure; if primarily internalized, the constraint is more stable but the identity fusion is deeper—suppression persists after legal penalties cease. This shifts the classification between tangled_rope (structural enforcement) and snare (internalized extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether dueling suppression operated through external barriers or internalized moral redefinition.').

omega_variable(
    reading_content_vs_kernel,
    'Is this reading (honor-redefinition-as-contraction) describing a genuine shift in what legitimacy claims could coherently be made, or describing selective enforcement that left alternative honor framings alive but suppressed?',
    'Textual and archival analysis: did intellectuals, reformers, and officials argue they were REDEFINING what honor means (contraction axiom: honor-and-violence-are-contradictory), or did they argue dueling was ILLEGAL and IMMORAL but not that honor-by-combat was conceptually incoherent?',
    'If the contraction was conceptual (the very idea of honor shifted), the reading is ε-complete. If enforcement was selective suppression while alternative framings remained available (just underground or illegal), the constraint is a snare, not a tangled_rope, and the sibling drop_reading or composite_reading better captures the structural dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_content_vs_kernel, conceptual, 'Whether the constraint operates via conceptual redefinition or via selective enforcement of existing illegality.').

omega_variable(
    kernel_boundary_disambiguation,
    'Is the kernel ''honor and violence'' or ''the definition of legitimate honor in society''? Does the contraction reading claim honor was redefined universally, or only in institutional/state contexts?',
    'Ethnographic and historical comparative analysis: did honor-via-combat persist in non-state communities, colonized societies, or peripheral populations? Did the redefinition apply equally or selectively by social position?',
    'If the redefinition was partial or selective, the sibling readings (drop, composite) may describe different social levels or contexts better than this reading. If dueling honor persisted anywhere while legal honor-via-civility dominated institutionally, the constraint is structured by level-resolved coercion, not universal conceptual contraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_boundary_disambiguation, conceptual, 'Whether the honor-redefinition was universal or stratified by institutional context and social position.').

omega_variable(
    vindication_vs_extraction,
    'Do the vindicated propositions (state monopoly on violence, civility-as-civilization, rational-law supremacy) describe genuine coordination functions the constraint enabled, or describe the narrative justification for extraction?',
    'Comparative analysis: did societies that maintained decentralized honor-combat systems suffer measurably worse outcomes in violence rates, institutional stability, or state capacity? Or did state violence-monopoly permit new forms of systemic violence (colonial warfare, state punishment, police coercion) not present in honor-combat systems?',
    'If the vindicated propositions track real coordination gains, the constraint is genuinely tangled_rope (coordination + extraction). If the propositions are primarily cover narratives for institutional power consolidation, the constraint is snare with theatrical justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vindication_vs_extraction, empirical, 'Whether the constraint''s vindicated propositions describe real coordination benefits or cover narratives for state extraction.').

omega_variable(
    kernel_reading_alternative,
    'How does this reading (contraction) differ from the sibling readings in structural claim, and could they operate simultaneously (coexist) or does one logically foreclose the others?',
    'Schema-level: examine the axiom sets and reference frames for each reading and determine whether they can be held in the same framework by different parties or whether adoption of one necessarily excludes the other.',
    'This question is resolved by the cs_structure.reading_relations declarations: if this reading forecloses a sibling, the kernel has genuine logical structure; if readings coexist or influence but don''t foreclose, the kernel is multiply interpretable and readings are live alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternative, conceptual, 'How this reading relates structurally to sibling readings in the honor-violence kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__contraction_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(hono_tr_t0, observed).
narrative_ontology:measurement(hono_tr_t5, honor_violence_legitimacy__contraction_reading, theater_ratio, 5, 0.23).
narrative_ontology:measurement_basis(hono_tr_t5, observed).
narrative_ontology:measurement(hono_tr_t10, honor_violence_legitimacy__contraction_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(hono_tr_t10, observed).
narrative_ontology:measurement(hono_tr_t15, honor_violence_legitimacy__contraction_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement_basis(hono_tr_t15, observed).
narrative_ontology:measurement(hono_tr_t25, honor_violence_legitimacy__contraction_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement_basis(hono_tr_t25, observed).
narrative_ontology:measurement(hono_tr_t40, honor_violence_legitimacy__contraction_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(hono_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__contraction_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(hono_be_t0, observed).
narrative_ontology:measurement(hono_be_t5, honor_violence_legitimacy__contraction_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(hono_be_t5, observed).
narrative_ontology:measurement(hono_be_t10, honor_violence_legitimacy__contraction_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(hono_be_t10, observed).
narrative_ontology:measurement(hono_be_t15, honor_violence_legitimacy__contraction_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(hono_be_t15, observed).
narrative_ontology:measurement(hono_be_t25, honor_violence_legitimacy__contraction_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(hono_be_t25, observed).
narrative_ontology:measurement(hono_be_t40, honor_violence_legitimacy__contraction_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(hono_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_violence_legitimacy__contraction_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(hono_su_t0, observed).
narrative_ontology:measurement(hono_su_t5, honor_violence_legitimacy__contraction_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement_basis(hono_su_t5, observed).
narrative_ontology:measurement(hono_su_t10, honor_violence_legitimacy__contraction_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(hono_su_t10, observed).
narrative_ontology:measurement(hono_su_t15, honor_violence_legitimacy__contraction_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement_basis(hono_su_t15, observed).
narrative_ontology:measurement(hono_su_t25, honor_violence_legitimacy__contraction_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(hono_su_t25, observed).
narrative_ontology:measurement(hono_su_t40, honor_violence_legitimacy__contraction_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(hono_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(hono_grid_01, honor_violence_legitimacy__contraction_reading, accessibility_collapse(class), 0, 0.68).
narrative_ontology:measurement(hono_grid_02, honor_violence_legitimacy__contraction_reading, accessibility_collapse(class), 40, 0.81).
narrative_ontology:measurement(hono_grid_03, honor_violence_legitimacy__contraction_reading, accessibility_collapse(individual), 0, 0.62).
narrative_ontology:measurement(hono_grid_04, honor_violence_legitimacy__contraction_reading, accessibility_collapse(individual), 40, 0.84).
narrative_ontology:measurement(hono_grid_05, honor_violence_legitimacy__contraction_reading, accessibility_collapse(organizational), 0, 0.71).
narrative_ontology:measurement(hono_grid_06, honor_violence_legitimacy__contraction_reading, accessibility_collapse(organizational), 40, 0.89).
narrative_ontology:measurement(hono_grid_07, honor_violence_legitimacy__contraction_reading, accessibility_collapse(structural), 0, 0.55).
narrative_ontology:measurement(hono_grid_08, honor_violence_legitimacy__contraction_reading, accessibility_collapse(structural), 40, 0.78).
narrative_ontology:measurement(hono_grid_09, honor_violence_legitimacy__contraction_reading, resistance(class), 0, 0.61).
narrative_ontology:measurement(hono_grid_10, honor_violence_legitimacy__contraction_reading, resistance(class), 40, 0.38).
narrative_ontology:measurement(hono_grid_11, honor_violence_legitimacy__contraction_reading, resistance(individual), 0, 0.72).
narrative_ontology:measurement(hono_grid_12, honor_violence_legitimacy__contraction_reading, resistance(individual), 40, 0.31).
narrative_ontology:measurement(hono_grid_13, honor_violence_legitimacy__contraction_reading, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(hono_grid_14, honor_violence_legitimacy__contraction_reading, resistance(organizational), 40, 0.25).
narrative_ontology:measurement(hono_grid_15, honor_violence_legitimacy__contraction_reading, resistance(structural), 0, 0.52).
narrative_ontology:measurement(hono_grid_16, honor_violence_legitimacy__contraction_reading, resistance(structural), 40, 0.41).
narrative_ontology:measurement(hono_grid_17, honor_violence_legitimacy__contraction_reading, stakes_inflation(class), 0, 0.45).
narrative_ontology:measurement(hono_grid_18, honor_violence_legitimacy__contraction_reading, stakes_inflation(class), 40, 0.72).
narrative_ontology:measurement(hono_grid_19, honor_violence_legitimacy__contraction_reading, stakes_inflation(individual), 0, 0.41).
narrative_ontology:measurement(hono_grid_20, honor_violence_legitimacy__contraction_reading, stakes_inflation(individual), 40, 0.76).
narrative_ontology:measurement(hono_grid_21, honor_violence_legitimacy__contraction_reading, stakes_inflation(organizational), 0, 0.48).
narrative_ontology:measurement(hono_grid_22, honor_violence_legitimacy__contraction_reading, stakes_inflation(organizational), 40, 0.79).
narrative_ontology:measurement(hono_grid_23, honor_violence_legitimacy__contraction_reading, stakes_inflation(structural), 0, 0.38).
narrative_ontology:measurement(hono_grid_24, honor_violence_legitimacy__contraction_reading, stakes_inflation(structural), 40, 0.61).
narrative_ontology:measurement(hono_grid_25, honor_violence_legitimacy__contraction_reading, suppression(class), 0, 0.47).
narrative_ontology:measurement(hono_grid_26, honor_violence_legitimacy__contraction_reading, suppression(class), 40, 0.73).
narrative_ontology:measurement(hono_grid_27, honor_violence_legitimacy__contraction_reading, suppression(individual), 0, 0.36).
narrative_ontology:measurement(hono_grid_28, honor_violence_legitimacy__contraction_reading, suppression(individual), 40, 0.68).
narrative_ontology:measurement(hono_grid_29, honor_violence_legitimacy__contraction_reading, suppression(organizational), 0, 0.51).
narrative_ontology:measurement(hono_grid_30, honor_violence_legitimacy__contraction_reading, suppression(organizational), 40, 0.77).
narrative_ontology:measurement(hono_grid_31, honor_violence_legitimacy__contraction_reading, suppression(structural), 0, 0.39).
narrative_ontology:measurement(hono_grid_32, honor_violence_legitimacy__contraction_reading, suppression(structural), 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__contraction_reading, 0.12).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__composite_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, state_monopoly_on_violence_legitimation).

% DUAL FORMULATION NOTE:
% This story is one reading of a tripartite kernel decomposition. The honor-violence legitimacy kernel admits three structurally distinct readings: drop_reading (dueling rare due to external costs, not conceptual change), composite_reading (overdetermined: both drop and contraction), and this contraction_reading (honor conceptually redefined, violence expelled from legitimate set). Each reading generates its own constraint story with its own ε, stakeholder structure, and classification. They are linked by network.affects_constraints because the readings share a referent (the kernel 'honor and violence') but diverge on whether the constraint operates via enforcement (drop) or redefinition (contraction) or both (composite). The three readings represent live alternatives in historical sociology and legal anthropology; none forecloses the others universally, but the contraction reading's core axiom (honor-and-violence-contradictory) forecloses the drop reading's axiom (honor-and-violence-compatible) within any single framework. Empirical resolution requires distinguishing whether practitioners suppressed by law also internalized the redefinition or merely abandoned practice under duress.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_violence_legitimacy__contraction_reading, powerful, 0.82).
constraint_indexing:directionality_override(honor_violence_legitimacy__contraction_reading, moderate, 0.79).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
