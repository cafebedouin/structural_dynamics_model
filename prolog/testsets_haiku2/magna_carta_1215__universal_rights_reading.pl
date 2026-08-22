% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__universal_rights_reading, []).

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
 *   constraint_id: magna_carta_1215__universal_rights_reading
 *   human_readable: Magna Carta as Universal Due Process Constraint
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   Magna Carta, sealed by King John of England in 1215, was a feudal compact
 *   in its original institutional context: a covenant between the crown and
 *   landowning barons guaranteeing baronial property rights and limiting
 *   monarchical seizure. The universal-rights reading reinterprets 'free men'
 *   (originally a small class of property-holders) as all persons, and Clause
 *   39—'No freeman shall be arrested, imprisoned, disseized, outlawed,
 *   exiled, or in any other way ruined, nor will we attack him or send anyone
 *   to attack him, except by the lawful judgment of his peers or by the law
 *   of the land'—as a transhistorical constraint on arbitrary state power
 *   over any individual. This reading dominates liberal-democratic
 *   jurisprudence and rights advocacy, making it the institutional-seat
 *   default in those jurisdictions. The constraint operates as a rule-of-law
 *   coordinate: state power is subordinate to lawful process, and courts are
 *   the gatekeepers of lawfulness. Over eight centuries, this reading has
 *   shifted from a narrow feudal contract to a universal principle animating
 *   constitutional due-process protections globally. The tension between the
 *   reading's claim (universal applicability) and its actual enforcement
 *   (variable by regime, unequally implemented) produces the measured theater
 *   and suppression data.
 *
 * KEY AGENTS:
 *   - all_natural_persons: beneficiary in the universal reading; formally protected from arbitrary state power by Clause 39 read universally (power: powerless, exit: trapped)
 *   - state_executive_authority: payer; loses the prerogative of arbitrary action; costs are variable (power: institutional, exit: constrained)
 *   - judiciary_and_independent_adjudicators: agenda setter; gatekeeps lawfulness; concentrates interpretive authority (power: institutional, exit: mobile)
 *   - rights_advocacy_movements: beneficiary and agenda-setter; both benefit from the constraint's precedential force and actively enforce it through litigation and political pressure (power: organized, exit: mobile)
 *   - authoritarian_regimes: payer in structural terms; bear the cost of the constraint either as operational restriction or as theater maintenance (power: institutional, exit: constrained)
 *   - baronial_privilege_reading_adherents: excluded from liberal-democratic legal discourse; would dispute the universalization as anachronistic (power: powerful, exit: trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__universal_rights_reading, 0.41).
domain_priors:suppression_score(magna_carta_1215__universal_rights_reading, 0.68).
domain_priors:theater_ratio(magna_carta_1215__universal_rights_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, extractiveness, 0.41).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__universal_rights_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__universal_rights_reading, "Magna Carta as Universal Due Process Constraint").
narrative_ontology:topic_domain(magna_carta_1215__universal_rights_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__universal_rights_reading, '35c9a887-90e4-4611-b5d9-a6d11ea5ca07').
narrative_ontology:cs_kernel_codification('35c9a887-90e4-4611-b5d9-a6d11ea5ca07', fixed_text).
narrative_ontology:cs_authority_grounding('35c9a887-90e4-4611-b5d9-a6d11ea5ca07', lineage).
narrative_ontology:cs_interpretation_layer_present('35c9a887-90e4-4611-b5d9-a6d11ea5ca07').
narrative_ontology:cs_reading_relation('35c9a887-90e4-4611-b5d9-a6d11ea5ca07', magna_carta_1215__baronial_privilege_reading, forecloses).
narrative_ontology:cs_reading_relation('35c9a887-90e4-4611-b5d9-a6d11ea5ca07', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('35c9a887-90e4-4611-b5d9-a6d11ea5ca07', foundational, all_persons_entitled_lawful_judgment).
narrative_ontology:cs_axiom_status(all_persons_entitled_lawful_judgment, holdable).
narrative_ontology:cs_axiom_grounding('35c9a887-90e4-4611-b5d9-a6d11ea5ca07', all_persons_entitled_lawful_judgment, deontological).
narrative_ontology:cs_axiom('35c9a887-90e4-4611-b5d9-a6d11ea5ca07', foundational, arbitrary_state_violence_illegitimate).
narrative_ontology:cs_axiom_status(arbitrary_state_violence_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('35c9a887-90e4-4611-b5d9-a6d11ea5ca07', arbitrary_state_violence_illegitimate, deontological).
narrative_ontology:cs_reference_frame('35c9a887-90e4-4611-b5d9-a6d11ea5ca07', universal_due_process_norm).
narrative_ontology:cs_drift_state('35c9a887-90e4-4611-b5d9-a6d11ea5ca07', contemporary_authoritarian_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('35c9a887-90e4-4611-b5d9-a6d11ea5ca07', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__universal_rights_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, all_natural_persons).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, rights_advocacy_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, state_executive_authority).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, authoritarian_regimes).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, universal_human_dignity).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, rule_of_law_supremacy).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, arbitrary_power_illegitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The reading assigns every person under any state authority a right to due process: no detention or punishment without lawful judgment. This is a formal benefit with highly variable actual enforcement. In liberal democracies, the protection is institutionalized and largely functional; in authoritarian regimes, it is formal without enforcement. Persons cannot exit statehood to escape the constraint; it is structurally universal in scope.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, all_natural_persons, beneficiary,
    powerless, civilizational, trapped, universal).

% The constraint costs the executive the prerogative of arbitrary detention and punishment. In liberal democracies, the cost is bureaucratized into judicial review and procedural overhead. In authoritarian regimes, the constraint is acknowledged formally but costs are nominally borne through theater (maintaining the fiction of lawful process) while actual prerogative remains. The constraint structurally reduces the executive's unilateral power.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, state_executive_authority, payer,
    institutional, generational, constrained, universal).

% Judges and courts become the gatekeepers and arbiters of lawfulness under this reading. They gain institutional authority and prestige through the constraint's enforcement, but also bear responsibility for maintaining independence from executive pressure. Where courts are independent, the constraint is meaningful; where courts are captured by executive or legislative power, the constraint becomes theater. Courts can exit through institutional capture or through refusal to enforce (by invalidating the constraint), though the latter would be politically costly.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, judiciary_and_independent_adjudicators, agenda_setter,
    institutional, generational, mobile, universal).

% Rights movements (abolition, labor, anti-colonial, civil rights, human rights organizations) use Clause 39 and its reading as precedential authority to demand protection from arbitrary state violence. These movements both benefit from the constraint's existence and actively enforce it through litigation, advocacy, and political mobilization. They can exit by shifting focus to other constraints or frameworks, but the constraint's precedential force makes it a central strategic asset. The universal reading's scope benefits these movements by creating standing for any person to invoke protection.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, rights_advocacy_movements, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__universal_rights_reading, rights_advocacy_movements, agenda_setter).

% Regimes that rely on secret detention, extrajudicial killing, or political imprisonment experience the constraint as a formal liability but practical nothing. They may sign international agreements acknowledging the constraint (to maintain diplomatic standing) while systematically violating it domestically. The constraint costs them nothing operationally but costs them legitimacy if violations are exposed. They could exit by rejecting the universal reading outright (claiming Western imposition), but that carries legitimacy cost in global institutions.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, authoritarian_regimes, payer,
    institutional, biographical, constrained, universal).

% Scholars, historians, and a small number of jurists who argue the baronial reading (Magna Carta protects only landowning property holders, not all persons) are systematically excluded from the institutional conversation that the universal reading dominates in liberal democracies and global rights law. They have standing to speak in academic and historical contexts but lack institutional standing in courts and rights advocacy. They are trapped in the conversation because the charter itself is the object of dispute, and they cannot simply exit and use a different source.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, baronial_privilege_reading_adherents, excluded,
    powerful, generational, trapped, universal).

% Historians, comparative constitutional law scholars, and legal theorists observe the constraint's operation, the contest between its readings, and the gap between its universal claim and variable enforcement. This seat has no institutional stake in any reading's success or failure; it observes and analyzes.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes that state power is subordinate to lawful process: no person can be detained or punished without a lawful judgment arrived at through proceeding. This solves the foundational coordination problem of how any population can live under state authority without living in existential fear of arbitrary violence. By institutionalizing this constraint, the reading creates predictability: persons know they will not be arbitrarily seized, and state actors know the limits of their prerogative. The coordination is global: every state authority must abide by the same principle.
% TRANSFER_FUNCTION: Transfers the prerogative of arbitrary detention and punishment FROM state executives TO judges and lawful procedure. What moves is not material goods but political authority: the executive loses the unilateral right to act; instead, actors are bound to process and courts become gatekeepers. The reading asserts this transfer applies to all state/person relationships. In practice, the transfer is incomplete: authoritarian regimes maintain the prerogative formally while creating theatrical process; liberal democracies execute the transfer through institutional embedding; transitional regimes execute it inconsistently by venue and political salience.
% ABSENT_VOICES: Persons in authoritarian regimes where the constraint is theater; detainees in secret detention whose detention is formally denied (they are not in the conversation at all because they are not officially detained); enslaved and formerly enslaved people (the constraint coexisted with slavery in British colonies and the US for 150+ years, suggesting that 'free men' and 'all persons' diverged in application); historical persons the constraint excluded by race, caste, gender, or imperial status. The baronial reading also remains absent from liberal-democratic legal discourse as a live interpretive option, though it remains present in academic history. The living-document reading competes in the same institutional space (courts, legislatures) and is not absent—it is a co-inhabitor of institutional authority.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared—if courts lost gatekeeping authority and executives recovered the prerogative of arbitrary detention and punishment—the institutional world would fundamentally reorganize. In liberal democracies, the rule-of-law legitimacy regime would collapse; courts would lose their authority to review executive action; rights movements would lose precedential standing to demand due process. In authoritarian regimes, the facade of lawful process would drop; secret detention would become openly acknowledged; the constraint's presence as a formal international commitment would vanish. Transitional regimes would shift alignment toward authoritarian operation. The disappearance would rearrange the relationship between state authority and persons globally because the constraint is embedded in constitutional structures, international law, and institutional design worldwide. The reorganization would be toward either authoritarian-style executive prerogative (if the constraint simply vanished without replacement) or toward some alternative coordination mechanism (if a different constraint replaced it).
% FOUNDING_PROBLEM: In the feudal and early-modern period (roughly 11th–17th centuries), state authority—particularly monarchs and great nobles—claimed prerogative to seize property, imprison, and execute without constraint or proceeding. King John of England exemplified this: he seized baronial lands, imprisoned nobles, executed rivals, and demanded arbitrary fealty and tribute. The problem was acute for anyone with property or autonomy interests: they could be obliterated by executive fiat at any moment. Magna Carta (1215) was extracted as a covenant promising that at least some category of persons (originally construed as landowning barons) would have the right to legal judgment before forfeiture or death. The problem the constraint was built to solve is: how can anyone live under a power that can destroy them by whim?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by contemporaneous records (Angevin administrative documents, baronial correspondence, John's documented seizures of baronial lands and persons). Modern scholars studying the medieval period (e.g., David Carpenter, J.C. Holt) corroborate that arbitrary seizure and execution were real problems for barons in 13th-century England. The scope of the problem (whether it was limited to property disputes among elites or extended to violence against all persons) is the live contest: baronial historians argue the problem was feudal and elite-specific; universalist readings argue the logic of arbitrary power vs. rule of law applies transhistorically to all persons; living-document readings argue the problem evolved as societies modernized. Rights movements and liberal-democratic legal systems attest the universal reading as operative in their law; they effectively claim the founding problem is still live (arbitrary power remains a threat that law must constrain). Authoritarian regimes and critical scholars attest the problem is either dead in wealthy democracies (making the constraint partly ceremonial) or misapplied to non-Western contexts where other power structures operate. No corroboration exists outside the constraint's own beneficiary set (judiciary, rights movements, liberal democracies) for the claim that the universal reading is historically accurate to the 1215 charter's original intent; the corroboration comes from institutional actors whose authority the reading strengthens.
narrative_ontology:disappearance_verdict(magna_carta_1215__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__universal_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__universal_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_1215__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__universal_rights_reading, 0.41, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__universal_rights_reading_tests).
:- end_tests(magna_carta_1215__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate (0.41 at interval end) because the reading articulates a real constraint on state power but that constraint is unevenly enforced and differentially absorbed: liberal democracies bear it as bureaucratic overhead; authoritarian regimes treat it as costless theater; transitional regimes selectively enforce it. The reading itself claims minimal extraction (it is presented as pure coordination—a rule protecting all persons), but the measurement reflects the gap between claim and enforcement. Suppression rises from 0.45 (early period, when enforcement was sparse and baronial reading dominant) to 0.68 (modern period, when the universal reading is institutionalized but must actively suppress the baronial reading and selectively ignore violations by authoritarian regimes). Theater ratio falls from 0.85 (period when Magna Carta was invoked ceremonially but rarely enforced) to 0.52 (modern period, where enforcement is systematic in liberal democracies but theater in authoritarian contexts). The constraint's extractiveness stabilizes in the modern period (post-1700) as liberal institutions embed it; the suppression requirement plateaus as the reading becomes canonical in Western law and alternative readings are marginalized. Accessibility collapse is high (0.72) because once the universal-rights reading is institutionalized, alternatives become difficult to articulate without appearing to endorse arbitrary power. Resistance remains moderate (0.58) because authoritarian regimes and scholarly critics actively contest the universalization and enforcement, generating continuous pressure against the constraint's institutional dominance.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary and liberal-state seats, the constraint is a coordination mechanism that benefits all persons by limiting state prerogative and creating institutional predictability. From the authoritarian regime seat, it is a costless ceremonial commitment (theater = 0.85 early, 0.52 late) that can be acknowledged formally while violated operationally. From the powerless person's seat, the constraint is real and binding in liberal jurisdictions but absent in practice in authoritarian contexts, creating vast divergence in effective protection across space. The rights-advocacy seat reads the constraint as enormously extractive FROM state authority (the executive loses arbitrary prerogative) with minimal cost to the beneficiary (protection is costless to those protected). The state-executive seat reads it conversely: high cost in procedural overhead and loss of discretion; diffuse benefit distributed among powerless persons who would otherwise face arbitrary violence. The engine's per-seat computation will capture this divergence through directionality: judges and advocates sit near the beneficiary end; executives sit near the target end.
 *
 * DIRECTIONALITY LOGIC:
 *   The universal-rights reading creates a structural asymmetry: the beneficiary set (all persons) is universal and powerless, unable to exit statehood; the payer set (state authority) is concentrated, powerful, but structurally constrained by the need for a legitimate judicial process. Directionality for all_natural_persons: d ≈ 0.0–0.2 (full beneficiary; gains protection without bearing enforcement cost; trapped exit amplifies beneficiary position). Directionality for state_executive_authority: d ≈ 0.8–1.0 (full target; loses prerogative; constrained exit keeps the burden on the state). Directionality for judiciary_and_independent_adjudicators: d ≈ 0.5 (symmetric; they gain institutional authority and prestige, but bear the responsibility and political pressure to maintain independence). Directionality for rights_advocacy_movements: d ≈ 0.1–0.3 (beneficiary; they use the constraint's precedential force; mobile exit means they can withdraw if captured). Directionality for authoritarian_regimes: d ≈ 1.0 in formal law, d ≈ 0.0 in practice (the reading constrains them formally but theater allows them to bear no actual cost; the measurement tries to track actual burden, not formal position). Directionality for baronial_privilege_reading_adherents: excluded stakeholders have no directionality (they are kept out of the institutional conversation). The measurement (extractiveness = 0.41) reflects the weighted-average effective burden: liberal jurisdictions distribute the cost as procedural overhead (moderate extraction); authoritarian regimes bear none (zero extraction); transitional regimes occupy intermediate positions. The 'claim/metric independence' rule applies here: I claim the constraint is rope (pure coordination) because the reading's frame is that Clause 39 solves the foundational problem of arbitrary power through law. The metrics reflect that this reading is embedded in an institutional structure that partially suppresses alternatives (suppression = 0.68) and requires active enforcement (requires_active_enforcement = true), which pushes empirical operation toward tangled-rope or snare in authoritarian contexts. The engine will compute the per-seat type; the divergence between my claim (rope) and any computed type (tangled_rope in executive seats, mountain in powerless seats) is the measurement the corpus exists to take.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (arbitrary state power threatens subjects) is still structurally live in authoritarian regimes (status = live in authoritarian seats) and contested in liberal democracies (status = contested in liberal seats regarding whether the founding problem justifies the modern scope). The disappearance verdict is world_rearranges: if the constraint vanished, institutions would reorganize. But there is a mandatrophy risk: in liberal democracies where courts embed the constraint as routine procedure, the founding problem (existential fear of arbitrary violence) may have atrophied or shifted; the constraint persists to legitimize judicial power and rule-of-law ideology, not because subjects would face arbitrary violence without it. In this reading, the constraint has partially outlived its function in wealthy liberal democracies (where law enforcement and bureaucracy provide de facto predictability even without explicit due-process philosophy) while remaining live in authoritarian contexts. A mandatrophy declaration would be: the constraint is functionally vital in authoritarian contexts and transitional regimes but partly ceremonial in liberal democracies where stable bureaucracy provides the same protection. This does not invalidate the constraint—it clarifies the functional divergence by seat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_vs_feudal_interpretation,
    'Does ''free men'' in the 1215 charter refer to a narrow class of property-holding barons or to all persons in a transhistorical sense?',
    'Paleographic and contextual analysis of the charter''s drafting, comparison with other 13th-century feudal documents, analysis of how ''free men'' was understood and used in contemporary legal contexts, examination of which groups were explicitly excluded or included in the charter''s original application.',
    'If ''free men'' is feudal-class-specific, the constraint applies only to property disputes and the beneficiary set is narrow (baronial_privilege_reading dominates); if the term carries or acquired universal meaning, the constraint''s beneficiary set expands to all persons and the reading supports modern due-process doctrine. This determines whether the constraint is a feudal coordination mechanism (low extractiveness, narrow scope) or a universal rights principle (extractiveness depends on enforcement, universal scope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universal_vs_feudal_interpretation, empirical, 'Historical interpretation of the charter''s scope and language.').

omega_variable(
    institutional_embedding_vs_natural_enforcement,
    'Is the suppression requirement measured (0.68) a necessary feature of enforcing the constraint, or does it reflect institutional capture and the suppression of alternative readings?',
    'Comparative analysis of due-process protections in jurisdictions with different institutional structures (adversarial vs. inquisitorial, common-law vs. civil-law, weak vs. strong judiciaries); measurement of actual due-process violations and enforcement rates by regime type; analysis of whether suppression of alternative readings (baronial, contextual) serves the constraint''s legitimate function or protects institutional turf.',
    'If suppression is necessary to the constraint''s function (courts must suppress executive override-attempts), the constraint is a genuine tangled_rope (coordination + enforcement). If suppression reflects institutional gatekeeping that excludes legitimate alternatives, the constraint''s extraction is higher than the measurement suggests and it trends toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_embedding_vs_natural_enforcement, empirical, 'Whether measured suppression serves the constraint''s function or institutional power consolidation.').

omega_variable(
    north_american_slavery_exception,
    'Why did the universal-rights reading coexist with slavery and slave extraction in the British colonies and United States for 150+ years after Magna Carta became canonical in English law?',
    'Analysis of legal doctrine in colonial and early-American law regarding whether enslaved people were ''free men'' under Clause 39; examination of judicial decisions that excluded or included enslaved people from due-process protection; analysis of the reading''s scope as geographically and racially bounded despite universalist framing.',
    'If the constraint''s universalism was always understood to exclude enslaved people and other subaltern groups, the measured beneficiary set is effectively smaller than the reading claims (making the constraint less universalist than authored); if the exclusion was a later rationalization contradicting the reading''s core premise, the constraint''s history shows systematic suppression of its own implications. Either way, it demonstrates that the constraint''s claimed universalism diverges from its actual scope in practice, raising questions about whether universalism is intrinsic to the constraint or an interpretation layered over historical operation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(north_american_slavery_exception, conceptual, 'Whether the constraint''s universalism was historically honored or systematically violated and rationalized.').

omega_variable(
    committer_frame_baronial_vs_universal,
    'Is the universal-rights reading a discovery of Clause 39''s true meaning, or a deliberate reinterpretation that suppresses the original baronial reading to expand judicial and rights-advocacy power?',
    'Historical analysis of when and why the universalist reading displaced the baronial reading in English jurisprudence; examination of incentives for the judiciary and rights movements to promote universalism (does it strengthen their institutional position?); assessment of whether the universal reading is the most straightforward interpretation of the 1215 text or requires 600+ years of precedential accretion to become plausible.',
    'If the reading is a discovery, the constraint''s operation reflects application of an objective principle (rope/coordination). If it is a strategic reinterpretation, the constraint''s extraction is higher than acknowledged because the beneficiaries (judiciary, rights-advocacy institutions) use the reading to expand their authority and legitimacy, making it a snare disguised as a rope. This omega directly addresses whether the committer frame (the reading as a deliberate choice among live alternatives) is historically accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_baronial_vs_universal, conceptual, 'Whether universalist reading is discovery or strategic institutional reinterpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__universal_rights_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_1215__universal_rights_reading, theater_ratio, 0, 0.85).
narrative_ontology:measurement_basis(magn_tr_t0, observed).
narrative_ontology:measurement(magn_tr_t100, magna_carta_1215__universal_rights_reading, theater_ratio, 100, 0.78).
narrative_ontology:measurement_basis(magn_tr_t100, observed).
narrative_ontology:measurement(magn_tr_t200, magna_carta_1215__universal_rights_reading, theater_ratio, 200, 0.71).
narrative_ontology:measurement_basis(magn_tr_t200, observed).
narrative_ontology:measurement(magn_tr_t300, magna_carta_1215__universal_rights_reading, theater_ratio, 300, 0.65).
narrative_ontology:measurement_basis(magn_tr_t300, observed).
narrative_ontology:measurement(magn_tr_t500, magna_carta_1215__universal_rights_reading, theater_ratio, 500, 0.54).
narrative_ontology:measurement_basis(magn_tr_t500, observed).
narrative_ontology:measurement(magn_tr_t700, magna_carta_1215__universal_rights_reading, theater_ratio, 700, 0.52).
narrative_ontology:measurement_basis(magn_tr_t700, observed).
narrative_ontology:measurement(magn_tr_t800, magna_carta_1215__universal_rights_reading, theater_ratio, 800, 0.52).
narrative_ontology:measurement_basis(magn_tr_t800, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_1215__universal_rights_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(magn_be_t0, observed).
narrative_ontology:measurement(magn_be_t100, magna_carta_1215__universal_rights_reading, base_extractiveness, 100, 0.22).
narrative_ontology:measurement_basis(magn_be_t100, observed).
narrative_ontology:measurement(magn_be_t200, magna_carta_1215__universal_rights_reading, base_extractiveness, 200, 0.28).
narrative_ontology:measurement_basis(magn_be_t200, observed).
narrative_ontology:measurement(magn_be_t300, magna_carta_1215__universal_rights_reading, base_extractiveness, 300, 0.35).
narrative_ontology:measurement_basis(magn_be_t300, observed).
narrative_ontology:measurement(magn_be_t500, magna_carta_1215__universal_rights_reading, base_extractiveness, 500, 0.4).
narrative_ontology:measurement_basis(magn_be_t500, observed).
narrative_ontology:measurement(magn_be_t700, magna_carta_1215__universal_rights_reading, base_extractiveness, 700, 0.41).
narrative_ontology:measurement_basis(magn_be_t700, observed).
narrative_ontology:measurement(magn_be_t800, magna_carta_1215__universal_rights_reading, base_extractiveness, 800, 0.41).
narrative_ontology:measurement_basis(magn_be_t800, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_1215__universal_rights_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(magn_su_t0, observed).
narrative_ontology:measurement(magn_su_t100, magna_carta_1215__universal_rights_reading, suppression_requirement, 100, 0.52).
narrative_ontology:measurement_basis(magn_su_t100, observed).
narrative_ontology:measurement(magn_su_t200, magna_carta_1215__universal_rights_reading, suppression_requirement, 200, 0.58).
narrative_ontology:measurement_basis(magn_su_t200, observed).
narrative_ontology:measurement(magn_su_t300, magna_carta_1215__universal_rights_reading, suppression_requirement, 300, 0.62).
narrative_ontology:measurement_basis(magn_su_t300, observed).
narrative_ontology:measurement(magn_su_t500, magna_carta_1215__universal_rights_reading, suppression_requirement, 500, 0.67).
narrative_ontology:measurement_basis(magn_su_t500, observed).
narrative_ontology:measurement(magn_su_t700, magna_carta_1215__universal_rights_reading, suppression_requirement, 700, 0.68).
narrative_ontology:measurement_basis(magn_su_t700, observed).
narrative_ontology:measurement(magn_su_t800, magna_carta_1215__universal_rights_reading, suppression_requirement, 800, 0.68).
narrative_ontology:measurement_basis(magn_su_t800, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__universal_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__universal_rights_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, magna_carta_1215__baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, magna_carta_1215__living_document_reading).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, common_law_due_process_principle).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, us_fifth_amendment_due_process).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, european_convention_article_5).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Magna Carta kernel (magna_carta_1215). The universal-rights reading interprets 'free men' and Clause 39 as universal, benefiting all persons equally. The baronial_privilege_reading constrains scope to landowning barons. The living_document_reading treats the constraint as evolving through interpretive tradition rather than fixed. All three readings affect downstream due-process constraints (US Fifth Amendment, European Convention Article 5, common-law principles) but instantiate different scope and beneficiary sets. The universal-rights reading is the dominant institutional instantiation in liberal democracies and global rights law; it shapes and constrains the other readings' credibility in that discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
