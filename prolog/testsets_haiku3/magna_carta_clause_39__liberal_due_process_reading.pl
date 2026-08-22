% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__liberal_due_process_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__liberal_due_process_reading, []).

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
 *   constraint_id: magna_carta_clause_39__liberal_due_process_reading
 *   human_readable: Magna Carta Clause 39: Liberal Due Process Reading
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   Clause 39 of Magna Carta (1215) reads: 'No free man shall be seized or
 *   imprisoned, or stripped of his rights or possessions, except by the
 *   lawful judgment of his equals or by the law of the land.' The liberal due
 *   process reading expands this narrow procedural guarantee into a universal
 *   principle: all individuals hold inherent rights against arbitrary state
 *   power, and the state's authority is fundamentally limited by law, not by
 *   grace. This reading animates centuries of constitutional development —
 *   from common law due process to 17th-century declarations of rights to
 *   modern constitutional review. It asserts that Clause 39's force is not
 *   merely feudal-procedural (a baron's right to trial by peers before
 *   execution) but foundational to a rights-bearing conception of personhood.
 *   The constraint operates on states as the primary target: it suppresses
 *   the Crown's unilateral discretion and requires it to justify acts by law
 *   and submit to judicial review. Individual beneficiaries (all subjects,
 *   especially property owners and merchants) gain standing and protection.
 *   The claim/metric divergence is intentional: this reading CLAIMS the
 *   constraint is tangled_rope (real coordination function — the judiciary as
 *   neutral arbiter — plus asymmetric extraction because the Crown bears
 *   disproportionate suppressive burden while the judiciary and property
 *   owners benefit). The metrics (extractiveness 0.68, suppression 0.72,
 *   theater 0.41) describe substantial enforcement burden and rising
 *   performative behavior over the long interval, consistent with a
 *   constraint that must be continually defended against erosion.
 *
 * KEY AGENTS:
 *   - individual_subjects: powerless seat; gain legal standing against arbitrary Crown action; trapped exit (cannot leave jurisdiction)
 *   - property_owners: powerful seat; capture most substantive benefit (property protection); constrained exit (can relocate but prefer to stay and litigate)
 *   - Crown/royal administration: institutional payer; loses discretion; dual role as both enforcer (formally names itself bound) and target of enforcement (courts check its acts)
 *   - Judiciary: institutional agenda-setter; becomes co-sovereign authority determining legality; analytical position (neither collecting nor bearing direct costs, but essential to the constraint's operation)
 *   - Feudal prerogative holders: institutional payer; lose customary discretion to extract without process; represent the sibling reading's beneficiary set
 *   - Royal-feudal hierarchy (excluded): would argue Clause 39 dissolves legitimate hierarchical order; absence from the reading's framing is constitutive
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, 0.68).
domain_priors:suppression_score(magna_carta_clause_39__liberal_due_process_reading, 0.72).
domain_priors:theater_ratio(magna_carta_clause_39__liberal_due_process_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, accessibility_collapse, 0.44).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__liberal_due_process_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__liberal_due_process_reading, "Magna Carta Clause 39: Liberal Due Process Reading").
narrative_ontology:topic_domain(magna_carta_clause_39__liberal_due_process_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__liberal_due_process_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__liberal_due_process_reading, 'b1bc3d88-1715-40d1-9e0b-b9f37b27a55b').
narrative_ontology:cs_kernel_codification('b1bc3d88-1715-40d1-9e0b-b9f37b27a55b', fixed_text).
narrative_ontology:cs_authority_grounding('b1bc3d88-1715-40d1-9e0b-b9f37b27a55b', lineage).
narrative_ontology:cs_interpretation_layer_present('b1bc3d88-1715-40d1-9e0b-b9f37b27a55b').
narrative_ontology:cs_reading_relation('b1bc3d88-1715-40d1-9e0b-b9f37b27a55b', magna_carta_clause_39__feudal_prerogative_reading, forecloses).
narrative_ontology:cs_reading_relation('b1bc3d88-1715-40d1-9e0b-b9f37b27a55b', magna_carta_clause_39__originalist_limitation_reading, influences).
narrative_ontology:cs_axiom('b1bc3d88-1715-40d1-9e0b-b9f37b27a55b', foundational, universal_individual_rights_against_arbitrary_power).
narrative_ontology:cs_axiom_status(universal_individual_rights_against_arbitrary_power, holdable).
narrative_ontology:cs_axiom_grounding('b1bc3d88-1715-40d1-9e0b-b9f37b27a55b', universal_individual_rights_against_arbitrary_power, deontological).
narrative_ontology:cs_axiom('b1bc3d88-1715-40d1-9e0b-b9f37b27a55b', foundational, legal_process_binds_sovereign_authority).
narrative_ontology:cs_axiom_status(legal_process_binds_sovereign_authority, holdable).
narrative_ontology:cs_axiom_grounding('b1bc3d88-1715-40d1-9e0b-b9f37b27a55b', legal_process_binds_sovereign_authority, deontological).
narrative_ontology:cs_reference_frame('b1bc3d88-1715-40d1-9e0b-b9f37b27a55b', rule_of_law_over_discretionary_authority).
narrative_ontology:cs_drift_state('b1bc3d88-1715-40d1-9e0b-b9f37b27a55b', contemporary_rights_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b1bc3d88-1715-40d1-9e0b-b9f37b27a55b', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, individual_subjects).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, property_owners).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, merchant_class).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, arbitrary_royal_authority).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, feudal_prerogative_holders).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, crown_administrative_discretion).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, universal_individual_rights_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, rule_of_law_over_arbitrary_power).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, due_process_precedent_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under the liberal reading, all subjects hold inherent rights against arbitrary state action: no imprisonment without lawful judgment, no seizure without due process. They gain legal standing to contest state acts and can appeal to courts for redress. Their situation is formally empowered (by law and rights-language) but materially constrained (by their lack of resources to litigate and by institutional bias toward Crown). They cannot exit the kingdom without abandoning citizenship; they must submit to process, but the process is theoretically open to them.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, individual_subjects, beneficiary,
    powerless, biographical, trapped, national).

% Gain explicit protection for lands and chattels against arbitrary Crown distraint or forfeiture. They hold the resources and standing to bring suits in courts and can afford to hire advocates. The liberal reading shields their property from the Crown's revenue-raising discretion, which was the primary grievance of 1215 barons. They benefit disproportionately because courts are accessible to them and because property rights are legible in law. Exit is constrained but available (they can relocate if protection fails, though most prefer to stay and litigate for their rights).
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, property_owners, beneficiary,
    powerful, generational, constrained, national).

% Gain predictability for commercial activity and protection against arbitrary Crown seizure, monopoly revocation, or market interference. The liberal reading prevents the Crown from arbitrarily annulling commercial charters or confiscating goods. Merchants benefit from the rule of law because commerce depends on future certainty. They have moderate resources and can sometimes litigate but often choose to relocate if the constraint erodes. Their mobility gives them a better exit option than landowners.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, merchant_class, beneficiary,
    moderate, biographical, mobile, national).

% The Crown must justify acts through legal process and submit to judicial review. It loses the ability to imprison, seize property, or revoke rights unilaterally. It is nominally the enforcer of Clause 39 (it agreed to Magna Carta and in theory enforces it) but is also the target (courts check its actions). This dual role captures the paradox of the constraint: the Crown is bound by law to an authority (courts) that can nullify its acts. The Crown bears the suppressive force of judicial review and loss of absolute prerogative. Its constrained exit reflects that it cannot simply abandon legal process without losing legitimacy.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, crown_administrative_discretion, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__liberal_due_process_reading, crown_administrative_discretion, agenda_setter).

% Barons, bishops, and feudal magnates lose their customary right to extract rents, wardship, reliefs, and labor from their inferiors without legal process. Under the feudal prerogative reading, they held legitimate discretion within the hierarchy; the liberal reading classifies that discretion as arbitrary and suppresses it. They represent the sibling reading's beneficiary set — the constraint is a direct extraction from their traditional power. They are forced to submit their acts to legal adjudication, which is expensive and unpredictable. Their exit is constrained (they cannot abandon their lands and titles without losing status).
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, feudal_prerogative_holders, payer,
    institutional, generational, constrained, national).

% Courts become the enforcers and interpreters of Clause 39. They adjudicate claims that state or feudal action violates due process, they can nullify unlawful acts, and they develop common-law doctrine around 'law of the land.' The liberal reading elevates the judiciary to co-sovereign authority, though this is not explicit in the clause itself. The courts must bear the burden of adjudication, withstand political pressure, and maintain independence from Crown and feudal powers. They are neither purely beneficiary nor purely payer: they gain authority but also responsibility and vulnerability. Analytically positioned: they observe the constraint's operation and adjudicate disputes, but their role is not identical to other stakeholders.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, judicial_authority, agenda_setter,
    institutional, generational, analytical, national).

% The feudal estate system, organized as a hierarchy of reciprocal duties and customary prerogatives, would resist the liberal reading's universalization of individual rights. They see Clause 39 as a disruption of the organic social order and an illegitimate expansion of royal authority (courts) into feudal matters. They are excluded because the liberal reading's framing does not recognize the legitimacy of their hierarchical order — it treats their discretion as arbitrary, not as legitimate. Their voices would argue that individual rights are anachronistic and that the clause should be read as preserving the hierarchy, not dissolving it. They remain excluded as long as the liberal reading holds institutional authority.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, royal_feudal_hierarchy, excluded,
    institutional, generational, trapped, national).

% Scholars and jurists who read Clause 39 as addressing only the specific 1215 abuses contest the liberal reading's expansion. They observe that the clause's language refers to 'free men,' 'lawful judgment of his peers,' and 'law of the land' — all terms embedded in feudal procedure — and that universalizing it to a general due process principle reads forward a meaning the framers could not have had. They note that the clause does not explicitly forbid prerogative, and they resist the liberal reading's reinterpretation as an assault on legitimate authority. Analytically positioned: they observe the constraint's interpretation history and advocate for textual bounds on expansion.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, originalist_interpreters, observer,
    analytical, civilizational, analytical, universal).

% Serfs, slaves, and unfree persons are entirely excluded from the liberal reading's protections. Clause 39 refers to 'free men' and the liberal reading does not extend universal individual rights to the unfree majority. They are excluded not by accident but by the reading's own framing: the reading's beneficiaries are propertied and free persons; the constraint redistributes power among them and against the Crown and feudal magnates, but it does not address the status of the unfree. Their absence is constitutive of the constraint and marks a critical limit on the liberal reading's claimed universalism.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, unfree_and_servants, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes an impartial legal process (courts, law, due process) to adjudicate disputes between subjects and state authority, replacing unilateral Crown discretion with rule-bound authority and judicial review. Solves the coordination problem that arises when a sovereign can act arbitrarily: by creating a neutral third party (judiciary) and a public law (knowable rules), the constraint allows subjects to predict state behavior and challenge violations. It coordinates the relationship between authority and rights-bearing persons.
% TRANSFER_FUNCTION: Transfers authority to determine legality from the Crown alone to a joint determination: the Crown proposes action, law constrains it, courts adjudicate challenges. It moves the power to imprison, seize, and revoke from unilateral discretion to process-bounded discretion. It transfers standing from the Crown (sole judge of its actions) to subjects (who can now sue). It transfers risk from subjects (who faced arbitrary loss) to the state (which faces judicial nullification of unlawful acts).
% ABSENT_VOICES: The feudal estate system would argue the clause should preserve hierarchical prerogative, not dissolve it into universal rights. The unfree would have nothing to say under the liberal reading because they are excluded — the reading offers them no protection and no standing. Originalists would argue the reading over-expands the clause beyond its 1215 intent. Executives and prerogative lawyers (in all eras) who invoke executive necessity and Crown immunity would argue for a narrower reading. These voices are kept out of the liberal framing by the reading's core commitment to universal individual rights and rule of law — the framing does not recognize legitimacy in claims for exemption, discretion, or status-hierarchy.
% DISAPPEARANCE_RATIONALE: If Clause 39 and its liberal reading disappeared, the Crown and feudal magnates would revert to arbitrary discretion. Subjects would have no standing to challenge imprisonment or property seizure. Courts would lose their authority to nullify unlawful acts. The entire subsequent development of English common law, constitutional limitation, parliamentary sovereignty, and individual rights doctrine depends on this clause. Its disappearance would require wholesale reconstruction of English and American constitutional law. The liberal reading is now so embedded in the legal tradition that reversing it would feel like the world rearranging itself — though technically the world existed before the reading was adopted, the modern legal order is built on the assumption that Clause 39 bounds authority.
% FOUNDING_PROBLEM: In 1215, the English Crown under King John exercised absolute discretion to imprison subjects indefinitely, seize and hold lands without returning them, exact arbitrary reliefs and wardship fees, revoke charters granted by previous monarchs, and exploit feudal incidents as pure revenue extraction. The baronage experienced this as arbitrary exaction with no recourse except force. The founding problem was the absence of any legal constraint on the Crown's ability to harm subjects and property owners.
% FOUNDING_PROBLEM_CORROBORATION: The Crown-as-institutional-actor attests the problem is solved — modern legal order has internalized the rule of law, courts are independent, arbitrary seizure is unthinkable, and Clause 39 is now theater. The judiciary and constitutional scholars attest the problem is still live in the sense that the constraint's enforcement is what prevents its return — that is, the constraint is necessary to keep the problem from re-emerging under political pressure. The originalist reading (the sibling constraint) attests the founding problem was specific to 1215 abuses and does not justify universal due process expansion. Independent testimony from civil-rights advocates confirms that the constraint remains vital: when executives invoke emergency powers or courts retreat from review, rights erosion follows. The shared testimony is that the constraint's disappearance would leave subjects unprotected.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__liberal_due_process_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__liberal_due_process_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__liberal_due_process_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_clause_39__liberal_due_process_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__liberal_due_process_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the liberal reading interprets Clause 39 as stripping the Crown and feudal magnates of absolute discretion — a massive transfer of power from unilateral authority to legally-constrained authority. The beneficiaries (individual subjects, property owners, merchants) extract this rights-allocation at the expense of the ancien régime's prerogative. Suppression (0.72) is elevated because the constraint requires continuous active enforcement: courts must check executive acts, legal arguments must be pressed, and the feudal prerogative must be actively suppressed by making it illegitimate. Theater (0.41) rises over the interval because as the constraint's initial force (13th-century enforcement against the Crown) fades and becomes embedded in custom, the Crown performs compliance and the constraint's operation becomes partly theatrical — the Crown is no longer actively threatened with civil war, but formal observance persists. Accessibility collapse (0.44) is moderate: alternatives to law-bound authority are not completely eliminated (absolute monarchy persists elsewhere, prerogative doctrines resurface), but within the English legal tradition, the alternative has become intellectually and institutionally inaccessible. Resistance (0.58) is substantial: the constraint meets continuous resistance from executive actors (Stuart kings invoke prerogative, modern governments assert emergency powers, courts face political pressure), which is why suppression must remain high. The measurement series shows the constraint's initial high enforcement cost and clear extractiveness degrading into higher theater as the constraint becomes normalized and less contested — a classic pattern for a constraint transitioning from actively-suppressed-rebellion to institutionalized-tradition.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence is sharp. From the Crown's position (institutional, constrained exit, long horizon), Clause 39 is a constraint on its authority — a forced coordination with the judiciary that strips unilateral power. From the property owner's position (powerful, but preferring constrained settlement to rebellion), it is a coordination mechanism that protects their assets and gives them recourse against arbitrariness — a rope, not a snare. From the individual subject's position (powerless, trapped), it is a purely extractive reading of existing feudal hierarchy, because the subject gains a right to legal process that the feudal system already nominally provided (trial by peers). The divergence arises because the liberal reading's framing of 'universal individual rights' is not a point of unanimity — the feudal reading reads the same Clause 39 as preserving estate-specific procedural rights within hierarchy, not granting universal personhood. The engine computes the Crown's and the feudal magnate's seats as payers; the property owner's seat as a beneficiary; the subject's seat as a beneficiary (but with low power and no exit, so their benefit is heavily suppressed). The liberal reading asserts all three payer seats SHOULD experience the constraint as binding, which is the force of its claim — but the computation will reveal whether this assertion survives the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown is the primary target (d near 1.0): it bears the full suppressive force of judicial review and loss of unilateral discretion. Individual subjects and property owners are beneficiaries (d near 0.0): they gain rights and standing without running the enforcement machinery. The judiciary sits near symmetric (d ≈ 0.5): they gain authority and institutional prestige but also bear the burden of adjudication and political pressure from the Crown and feudal powers trying to resist the constraint. The feudal prerogative holders are targets (d near 1.0): they lose customary discretion and face suppression of their claims. The merchant class is a moderate beneficiary (d ≈ 0.3): they gain security for commerce without bearing enforcement costs, though they may face taxation that funds judicial administration. No explicit directionality override is needed; the structural data (beneficiary/victim + exit options + power) derives d cleanly from the authored relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (arbitrary royal exaction, uncertain property tenure) is CONTESTED in status. The Crown's voice attests it is dead — modern legal order has internalized Clause 39's principle, the Crown no longer threatens arbitrary seizure, and current invocations of the clause are theater. The judiciary and property-owning scholars attest it is live — that the constraint's continued enforcement is what prevents the problem's return, and the problem re-emerges at every political crisis when executives test the boundaries of prerogative. The originalist reading (the sibling constraint) contests both: it asserts the founding problem was specific to 1215 feudal abuses, not a standing problem, and the liberal reading's expansion to a universal due process doctrine misreads the historical intent. The mandatrophy analysis resolves as follows: if founding_problem_status = dead AND the constraint persists, mandatrophy is present — the constraint has outlived its function and persists as custom/theater/path-dependence. If founding_problem_status = live, no mandatrophy (the constraint is still necessary). If contested, the mismatch depends on whether disappearance_verdict = world_rearranges: if the world rearranges, the founding problem is effectively live (its absence would be felt immediately), so the contested status is resolved toward 'live' for mandatrophy purposes. Here, disappearance_verdict = world_rearranges, so the constraint is classified as NOT mandatrophy despite the contested status on founding_problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_rights_vs_estate_hierarchy,
    'Does Clause 39 establish individual rights universally applicable to all subjects, or does it preserve estate-specific procedural rights within a legitimate feudal hierarchy?',
    'Historical interpretation of 1215 intent and subsequent judicial adoption: did the framers intend a principle (universal bounds on authority) or a remedy (protection against specific abuses of feudal incidents)?',
    'If individual rights are universal, the constraint applies to all state action against any person and grounds constitutional limitation (liberal reading). If estate-specific, the constraint preserves feudal hierarchy and applies only to free landholders, and the feudal prerogative reading is correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_rights_vs_estate_hierarchy, conceptual, 'Whether Clause 39 grounding is individual rights (universal) or status-based rights (hierarchical).').

omega_variable(
    law_of_the_land_scope,
    'Does ''law of the land'' mean the specific customs and written laws of England (historically bounded), or does it refer to discoverable universal principles of due process (trans-historically stable)?',
    'Jurisprudential analysis of how common-law courts have interpreted and applied ''law of the land'' over centuries: do they treat it as a fixed 1215 meaning or as an evolving principle?',
    'Historically-bounded: the originalist reading is correct, and expansions to modern constitutional review exceed the clause''s warrant. Evolutionarily open: the liberal reading is correct, and the clause grounds ongoing development of due process doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(law_of_the_land_scope, conceptual, 'Whether ''law of the land'' is a fixed historical meaning or an evolving principle.').

omega_variable(
    theater_accumulation_mechanism,
    'Does the rising theater_ratio over the 800-year interval reflect the constraint''s normalization (success: it has become so embedded that performance suffices), or does it indicate the constraint''s hollowing (failure: compliance becomes purely formal while real discretion returns)?',
    'Comparative analysis of judicial enforcement patterns across centuries: are courts actively nullifying state action, or have they retreated to ritual compliance? Measurement of actual Crown compliance vs. formal invocation.',
    'If normalization (success), the constraint is a Rope that has matured — high theater reflects stability. If hollowing (failure), the constraint is drifting toward Piton — the theater masks the return of arbitrary discretion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_accumulation_mechanism, empirical, 'Whether theater indicates constraint success (normalization) or failure (hollowing).').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.72) structural (external barriers: courts can be overridden, prerogative doctrines persist in law, executives test boundaries) or internalized (the Crown genuinely believes it is bound by law and voluntarily submits)?',
    'Behavioral observation during periods of executive pressure (wartime, emergency claims): does the Crown attempt to circumvent the constraint or accept judicial override? Do modern executives retreat when courts rule against them?',
    'If structural, the constraint requires continuous active enforcement and courts'' independence. If internalized, the Crown''s voluntary compliance suggests the constraint is stable without high suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression is external coercion or internalized legal norm.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the liberal reading of Clause 39 logically foreclose the feudal prerogative reading (both cannot be true in the same legal framework), or do they coexist as competing framings adopted by different parties?',
    'Formal logical analysis: can a single legal system hold both ''universal individual rights against arbitrary power'' (liberal) and ''hierarchical estate-specific procedural rights'' (feudal) without contradiction? Or is one the negation of the other?',
    'If foreclosed: one reading must be definitively rejected; the liberal reading''s triumph means the feudal reading is no longer live. If coexistent: both remain available; executives can invoke prerogative, courts invoke due process, and the oscillation between them is structural to the constraint''s operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the liberal and feudal readings are logically exclusive or structurally coexistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__liberal_due_process_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(magn_tr_t0, observed).
narrative_ontology:measurement(magn_tr_t100, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 100, 0.12).
narrative_ontology:measurement_basis(magn_tr_t100, observed).
narrative_ontology:measurement(magn_tr_t200, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 200, 0.18).
narrative_ontology:measurement_basis(magn_tr_t200, observed).
narrative_ontology:measurement(magn_tr_t400, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 400, 0.32).
narrative_ontology:measurement_basis(magn_tr_t400, observed).
narrative_ontology:measurement(magn_tr_t600, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 600, 0.38).
narrative_ontology:measurement_basis(magn_tr_t600, observed).
narrative_ontology:measurement(magn_tr_t800, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 800, 0.41).
narrative_ontology:measurement_basis(magn_tr_t800, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(magn_be_t0, observed).
narrative_ontology:measurement(magn_be_t100, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 100, 0.45).
narrative_ontology:measurement_basis(magn_be_t100, observed).
narrative_ontology:measurement(magn_be_t200, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 200, 0.52).
narrative_ontology:measurement_basis(magn_be_t200, observed).
narrative_ontology:measurement(magn_be_t400, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 400, 0.61).
narrative_ontology:measurement_basis(magn_be_t400, observed).
narrative_ontology:measurement(magn_be_t600, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 600, 0.66).
narrative_ontology:measurement_basis(magn_be_t600, observed).
narrative_ontology:measurement(magn_be_t800, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 800, 0.68).
narrative_ontology:measurement_basis(magn_be_t800, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(magn_su_t0, observed).
narrative_ontology:measurement(magn_su_t100, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 100, 0.48).
narrative_ontology:measurement_basis(magn_su_t100, observed).
narrative_ontology:measurement(magn_su_t200, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 200, 0.55).
narrative_ontology:measurement_basis(magn_su_t200, observed).
narrative_ontology:measurement(magn_su_t400, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 400, 0.64).
narrative_ontology:measurement_basis(magn_su_t400, observed).
narrative_ontology:measurement(magn_su_t600, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 600, 0.69).
narrative_ontology:measurement_basis(magn_su_t600, observed).
narrative_ontology:measurement(magn_su_t800, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 800, 0.72).
narrative_ontology:measurement_basis(magn_su_t800, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__liberal_due_process_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_clause_39__liberal_due_process_reading, 0.14).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39__feudal_prerogative_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39__originalist_limitation_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, habeas_corpus_writ_medieval_root).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, common_law_due_process_doctrine).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, parliamentary_sovereignty_constraint).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, constitutional_judicial_review).

% DUAL FORMULATION NOTE:
% The liberal due process reading and the feudal prerogative reading are two structurally distinct constraints instantiating the same text (Clause 39). They differ in scope (universal individual rights vs. estate-specific procedural rights), beneficiary structure (all subjects vs. feudal hierarchy), and victim set (arbitrary royal authority vs. legitimate prerogative). The extractiveness values diverge significantly: the liberal reading authors high extractiveness (0.68) because it strips prerogative holders of discretion; the feudal reading would author low extractiveness (near-zero) because it preserves customary hierarchy. They are linked by network.affects_constraints because the liberal reading's institutional triumph (adopted in English common law, American constitutional tradition) renders the feudal reading increasingly inaccessible, though never fully foreclosed. The originalist limitation reading sits upstream: it contests the liberal reading's universalizing move by recalling the specific 1215 context, suggesting the liberal reading over-reads the clause.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
