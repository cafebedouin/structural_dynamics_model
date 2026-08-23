% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__liberal_due_process_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: magna_carta_clause_39__liberal_due_process_reading
 *   human_readable: Magna Carta Clause 39 — Liberal Due Process Reading (Universal Individual Rights Against Arbitrary State Power)
 *   domain: legal/constitutional/political
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested kernel: clause 39 of
 *   Magna Carta (1215) as the liberal due process tradition reads it — a
 *   universal individual guarantee that no person shall be seized,
 *   imprisoned, dispossessed, outlawed, or exiled except by lawful judgment
 *   of equals or the law of the land. On this reading the clause binds every
 *   exercise of state coercive power, protects all persons subject to it, and
 *   stands as root stock of habeas corpus, due-process clauses, and
 *   fair-trial guarantees. The epsilon referent is the standing arrangement
 *   under contest — the process-bound exercise of state coercive power —
 *   assessed by this reading's own lights: the reading sees the removal of
 *   executive discretion as extensive and endorses it. The claim and the
 *   metrics are authored independently: the rope claim states this reading's
 *   structural verdict; the metrics describe the arrangement's actual
 *   operation across eight centuries, including the eras where practice
 *   sagged. Sibling readings of the same text are separate constraints, not
 *   positions inside this one; the contest is routed to the omegas and to
 *   cs_structure. KEY AGENTS (by structural relationship): -
 *   sovereign_executive: primary cost-bearer (institutional/identity_locked)
 *   — surrenders discretionary force; its legitimacy is constituted through
 *   the very limits that bind it - common_law_judiciary: administering
 *   authority (institutional/constrained) — issues the writs and defines
 *   lawful process; gains jurisdiction as the rule's reach widens -
 *   parliament_legislature: co-administering authority
 *   (institutional/constrained) — confirms and statutorily extends the
 *   promise - criminal_defendants: protected class (powerless/trapped) — the
 *   clause is their shield at the moment of maximum state pressure -
 *   political_dissidents: protected class (organized/trapped) — heaviest
 *   episodic users; invoke it precisely when enforcement is hardest -
 *   ordinary_households: protected class (moderate/constrained) — hold the
 *   rule as background insurance - unfree_tenants_and_the_enslaved: excluded
 *   voice (powerless/trapped) — governed by the legal order, outside its
 *   promise - extraterritorial_and_security_detainees: excluded voice
 *   (powerless/trapped) — modern populations held outside ordinary process -
 *   constitutional_historians: analytical observer (analytical/analytical) —
 *   see the full arc of text, interpretation, and practice
 *
 * KEY AGENTS:
 *   - sovereign_executive: primary cost-bearer (institutional/identity_locked) — surrenders discretionary force; its legitimacy is constituted through the very limits that bind it
 *   - common_law_judiciary: administering authority (institutional/constrained) — issues the writs and defines lawful process; gains jurisdiction as the rule's reach widens
 *   - parliament_legislature: co-administering authority (institutional/constrained) — confirms and statutorily extends the promise
 *   - criminal_defendants: protected class (powerless/trapped) — the clause is their shield at the moment of maximum state pressure
 *   - political_dissidents: protected class (organized/trapped) — heaviest episodic users; invoke it precisely when enforcement is hardest
 *   - ordinary_households: protected class (moderate/constrained) — hold the rule as background insurance
 *   - unfree_tenants_and_the_enslaved: excluded voice (powerless/trapped) — governed by the legal order, outside its promise
 *   - extraterritorial_and_security_detainees: excluded voice (powerless/trapped) — modern populations held outside ordinary process
 *   - constitutional_historians: analytical observer (analytical/analytical) — see the full arc of text, interpretation, and practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, 0.7).
domain_priors:suppression_score(magna_carta_clause_39__liberal_due_process_reading, 0.42).
domain_priors:theater_ratio(magna_carta_clause_39__liberal_due_process_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__liberal_due_process_reading, rope).
narrative_ontology:human_readable(magna_carta_clause_39__liberal_due_process_reading, "Magna Carta Clause 39 — Liberal Due Process Reading (Universal Individual Rights Against Arbitrary State Power)").
narrative_ontology:topic_domain(magna_carta_clause_39__liberal_due_process_reading, "legal/constitutional/political").

domain_priors:requires_active_enforcement(magna_carta_clause_39__liberal_due_process_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__liberal_due_process_reading, 'c00a3242-970c-47f5-89bd-02fa56a74c60').
narrative_ontology:cs_kernel_codification('c00a3242-970c-47f5-89bd-02fa56a74c60', fixed_text).
narrative_ontology:cs_authority_grounding('c00a3242-970c-47f5-89bd-02fa56a74c60', lineage).
narrative_ontology:cs_interpretation_layer_present('c00a3242-970c-47f5-89bd-02fa56a74c60').
narrative_ontology:cs_reading_relation('c00a3242-970c-47f5-89bd-02fa56a74c60', magna_carta_clause_39__feudal_prerogative_reading, forecloses).
narrative_ontology:cs_reading_relation('c00a3242-970c-47f5-89bd-02fa56a74c60', magna_carta_clause_39__originalist_limitation_reading, forecloses).
narrative_ontology:cs_axiom('c00a3242-970c-47f5-89bd-02fa56a74c60', foundational, lawful_judgment_precedes_deprivation).
narrative_ontology:cs_axiom_status(lawful_judgment_precedes_deprivation, holdable).
narrative_ontology:cs_axiom_grounding('c00a3242-970c-47f5-89bd-02fa56a74c60', lawful_judgment_precedes_deprivation, deontological).
narrative_ontology:cs_axiom('c00a3242-970c-47f5-89bd-02fa56a74c60', foundational, protection_extends_to_all_persons).
narrative_ontology:cs_axiom_status(protection_extends_to_all_persons, holdable).
narrative_ontology:cs_axiom_grounding('c00a3242-970c-47f5-89bd-02fa56a74c60', protection_extends_to_all_persons, deontological).
narrative_ontology:cs_reference_frame('c00a3242-970c-47f5-89bd-02fa56a74c60', universal_individual_rights_against_arbitrary_state).
narrative_ontology:cs_drift_state('c00a3242-970c-47f5-89bd-02fa56a74c60', contemporary_security_state_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c00a3242-970c-47f5-89bd-02fa56a74c60', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, criminal_defendants).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, political_dissidents).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, ordinary_households).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, sovereign_executive).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, rule_of_law_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, habeas_corpus_principle).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, limited_government_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The crown and its modern successors — presidents, ministries, security services — hold the coercive powers the clause reaches: arrest, imprisonment, dispossession, exile. Under this rule they may deploy none of them against any person except through lawful judgment of equals or the law of the land. What they surrender is discretionary force; what they retain is the ability to act, swiftly where lawful process permits. Leaving the arrangement would mean repudiating the constitutional order from which their own authority derives — the legitimacy they invoke is the same legitimacy that binds them.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, sovereign_executive, payer,
    institutional, generational, identity_locked, national).

% Judges issue the writs, hear the habeas petitions, and decide when state action crosses the line the clause draws. Each generation of courts reinterprets what lawful judgment and the law of the land require, extending the protection to new forms of state action. Their authority grows with the rule's reach; they cannot decline the jurisdiction without hollowing their own office.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, common_law_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Parliaments have confirmed, reissued, and statutorily extended the charter's promise — from the medieval confirmations through the seventeenth-century settlements to modern human-rights legislation. They set the statutory content of due process and can widen or narrow it by ordinary legislation within constitutional limits.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, parliament_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Anyone the state accuses: they face arrest, trial, imprisonment. The clause is their shield — it forces the state through open court, before judges and jurors, with notice and opportunity to respond. Without it, the accusation itself could be the punishment. Their protection depends on courts willing to hear them and counsel able to represent them.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, criminal_defendants, beneficiary,
    powerless, biographical, trapped, national).

% Those who oppose the government of the day: publishers, protesters, opposition figures. Historically the clause's most stressed users — they invoke it precisely when the executive most wants them gone. Their reliance on it is episodic and existential: they need it in the moments it is politically hardest to enforce.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, political_dissidents, beneficiary,
    organized, biographical, trapped, national).

% People with property, families, and no political profile. They rarely invoke the clause directly; they hold it as background insurance — the assurance that the state cannot simply take what they have or remove whom they love without process. Their stake is quiet but total.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, ordinary_households, beneficiary,
    moderate, generational, constrained, national).

% Historically, the vast unfree majority — villeins bound to the land, later the enslaved and transported — stood outside 'free man.' The clause's promise did not run to them; the same legal order that protected the free took their labor. Their descendants and successors pressed the boundary outward generation by generation.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, unfree_tenants_and_the_enslaved, excluded,
    powerless, generational, trapped, national).

% Modern detainees held outside ordinary criminal process — immigration custody, designated combatant status, emergency internment. The state argues the clause's protections do not reach them or yield to necessity; they are governed by state power while standing outside the courtroom the clause guarantees.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, extraterritorial_and_security_detainees, excluded,
    powerless, immediate, trapped, global).

% Scholars who trace the clause from the 1215 baronial settlement through Coke, the seventeenth-century settlements, and into modern constitutionalism. They see the whole arc — what the text said, what each age made of it, and where the promises and the practice part ways.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__liberal_due_process_reading, diffuse).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__liberal_due_process_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Makes every exercise of state coercive power pass through a public, reasoned, precedential process: arrests, seizures, and punishments must be authorized by lawful judgment of equals or the law of the land. This converts the sovereign's bare word from sufficient cause into insufficient cause, giving every subject — including elites and future officeholders — predictable security against the state's strongest member, and giving the state a legitimacy technology that raw force cannot supply.
% TRANSFER_FUNCTION: Moves coercive discretion out of executive hands and into judicially supervised process; moves security from the politically privileged to all persons the courts reach; moves procedural costs — courts, juries, counsel, delay — onto the state and onto litigants.
% ABSENT_VOICES: The unfree and the enslaved, who lived under the clause's legal order without its protection; colonized subjects governed by the crown abroad; and today's detainees held outside ordinary criminal process — immigration custody, designated-combatant status, emergency internment. Each would object that the promise of lawful judgment stops precisely at the populations state power finds most convenient to govern without it. They are absent from the courtroom and, historically, from the franchise.
% DISAPPEARANCE_RATIONALE: Habeas corpus, due-process clauses, fair-trial guarantees, and international arbitrary-detention prohibitions all descend from this clause's enforcement lineage. Overnight disappearance would return summary seizure and imprisonment to the executive toolkit, collapse the legitimacy structure modern constitutions borrow from it, and force every dependent doctrine to re-derive its authority from scratch — the constitutional world rearranges around the hole.
% FOUNDING_PROBLEM: King John's extrajudicial seizures: disseisin of land, imprisonment of debtors and opponents, mercenary exactions — taken without judgment of the man's peers or any settled law. The 1215 barons demanded that the king's coercive power run only through lawful judgment or the law of the land.
% FOUNDING_PROBLEM_CORROBORATION: No one attests the 1215 grievance itself — Angevin disseisin is dead. What is corroborated from outside the beneficiary set is the generalized problem: executives of every century since have attempted detention or seizure outside lawful process (the Star Chamber controversies, the seventeenth-century imprisonment debates, wartime internment, modern security detention), a record documented by constitutional historians including those hostile to the liberal reading, and conceded implicitly by every executive that invokes necessity. The persistence of attempted evasion is the external attestation that the founding problem never closed.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__liberal_due_process_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__liberal_due_process_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__liberal_due_process_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_clause_39__liberal_due_process_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__liberal_due_process_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.70 is authored from this reading's own lights about the standing arrangement — the process-bound exercise of state coercive power: the rule strips the executive of discretionary force across arrest, imprisonment, and dispossession, and the strip has deepened as courts extended lawful process to new forms of state action (trajectory: 0.62 at 1215, Tudor-era sag to 0.48, settlement recovery, twentieth-century extension to 0.76, partial security-state claw-back to 0.70). The reading endorses this removal; epsilon measures its magnitude, not its warrant. Suppression 0.42 is a raw structural property, unscaled by power or scope: the arrangement holds by constitutional entrenchment and active judicial enforcement, not by participant preference — the executive cannot vote itself out. Theater 0.22: the function is overwhelmingly real (writs issue, petitions are heard), with ritual residue in commemorative reaffirmation and in eras where confirmation ceremonies masked prerogative practice (the 1500 spike to 0.32). Accessibility_collapse 0.68: once the rule is understood, arbitrary governance collapses as a live option inside the constitutional order, though exception regimes keep partial access at the margins. Resistance 0.55: eight centuries of executive pushback — revocation, prerogative courts, emergency claims — met and absorbed. Claimed type rope is authored from structure, independent of these metrics: the arrangement solves a genuine collective-action problem (mutual security against the state's strongest member), its participants are net beneficiaries, and no seat captures what the executive pays. Receipt was checked seat by seat for gain_flow: the executive pays discretion and recoups legitimacy; the judiciary gains jurisdiction, not the surrendered discretion itself; protected classes receive the security the discretion converts into; no seat captures the flow as concentrated gain — hence the affirmative 'diffuse'. Fixing_cost 'prohibitive': removal would require dismantling the legitimacy structure every constituent institution draws its own authority and protection from; repair of coverage gaps varies by jurisdiction, but removal is categorically costly relative to any benefit.
 *
 * PERSPECTIVAL GAP:
 *   From the executive seat the same text computes as a fetter — high effective extraction against a party that cannot exit; from the defendant and household seats it computes as a shield — subsidy-shaped, near-negative effective extraction; from the judiciary seat it computes as a mandate that enlarges jurisdiction. The engine computes these divergent per-seat classifications from the structural data; this story's rope claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (criminal_defendants, political_dissidents, ordinary_households) derive low directionality — the arrangement subsidizes them with security. The sole victim declaration (sovereign_executive) derives high directionality, amplified by its identity-locked exit: a state cannot leave the constitutional order that constitutes it. Two corrections temper the picture and are left to the engine rather than forced through overrides: the executive recoups legitimacy, order, and elite security from the arrangement it pays into (omega executive_recoupment_share), so its true position sits short of full-target; and the judiciary, though an agenda-setter with no victim declaration, sits beneficiary-side — it gains jurisdiction as the rule widens. Directionality overrides in this schema are keyed only by power atom, and the three institutional seats (executive, judiciary, parliament) need DIFFERENT corrections — an atom-level override would distort the seats it does not name — so none is authored; the uncertainty is carried by the omegas instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem in its 1215 form (Angevin disseisin, debt imprisonment) is dead, but the generalized problem — executive discretion under necessity pressure — revives in every century, and each revival renews the arrangement's function rather than atrophying it. No mandatrophy resolution is declared: the mandate has been transformed, not outlived. The classification discipline cuts both ways here: a purely transfer-shaped read would classify the executive's loss as enforced extraction with a victim, missing the recouped legitimacy that keeps the executive short of full-target; a purely ceremonial read would take the commemorative theater (theater_ratio upticks at 1500 and in the commemorative eras) as evidence of vitality, missing the eras where ceremony masked real prerogative practice. The temporal series exists to catch both errors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_positioning,
    'This story authors the liberal_due_process_reading of kernel magna_carta_clause_39; the sibling readings (feudal_prerogative_reading, originalist_limitation_reading) would change the protected class, the referent of lawful process, and therefore epsilon and classification wholesale. Is the authored foreclosure assessment correct — do the liberal reading''s core premises (universal scope, rights held against the state, evolving referent) logically exclude both siblings within any single framework?',
    'Doctrinal analysis of whether any coherent single framework can hold the liberal scope-claim alongside estate-limited privilege or enumerated-abuse-only limitation; if a developmental framework legitimately holds the siblings diachronically (origin-as-privilege, present-function-as-right), downgrade the reading_relations to coexists_with.',
    'If foreclosure fails, the sibling readings become live alternatives within one framework and this story''s epsilon loses its invariance guarantee — the constraint would need re-authoring per framework rather than standing as one clean reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_positioning, conceptual, 'Kernel/reading positioning and foreclosure assessment for the clause-39 constraint family.').

omega_variable(
    universality_in_operation,
    'Does the universal protection hold in operation, or do exception regimes (emergency powers, immigration and security detention, extraterritorial custody) constitute a systematic coverage gap?',
    'Comparative data on detention outside ordinary process: rates of emergency-power invocation, noncitizen detention durations, habeas denial rates in security contexts, across jurisdictions and decades.',
    'If the gaps are systematic, the arrangement coordinates protection for the included while leaving the excluded governed without it — pressure toward a hybrid coordination/transfer reading with the excluded populations as payers; if episodic, the liberal reading''s coordination structure stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_in_operation, empirical, 'Whether universal coverage survives contact with exception regimes.').

omega_variable(
    executive_recoupment_share,
    'How much of what the executive surrenders under lawful-process requirements does it recoup as legitimacy, stability, and elite security — and does that recoupment place the executive''s structural position materially short of full-target?',
    'Historical-comparative analysis: survival and fiscal-capacity outcomes for regimes that accepted versus repudiated lawful-process limits; elite-cooperation records surrounding 1215 and the seventeenth-century settlements.',
    'Higher recoupment lowers the executive seat''s effective extraction and strengthens the net-beneficial coordination reading; near-zero recoupment pushes the executive toward full-target directionality and the arrangement toward enforced transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(executive_recoupment_share, empirical, 'Whether the paying seat recoups enough benefit to sit short of full-target directionality.').

omega_variable(
    law_of_the_land_referent,
    'What is the referent of ''law of the land'' and ''lawful judgment'' — fixed thirteenth-century content, hierarchical custom, or an evolving due-process standard?',
    'Doctrinal history: how courts across eras construed the phrase (Coke''s seventeenth-century gloss, due-process incorporation, modern proportionality review) and whether any construction commands cross-reading agreement.',
    'This is the load-bearing ambiguity separating all three sibling readings: a fixed referent collapses this reading toward the originalist sibling; a customary-hierarchical referent collapses it toward the feudal sibling; the evolving-standard referent is this reading''s distinctive commitment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(law_of_the_land_referent, conceptual, 'The referent of lawful process — the precise location of the disagreement among the sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__liberal_due_process_reading, 1215, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc39_liberal_dp_tr_t1215, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement_basis(mc39_liberal_dp_tr_t1215, observed).
narrative_ontology:measurement(mc39_liberal_dp_tr_t1500, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1500, 0.32).
narrative_ontology:measurement_basis(mc39_liberal_dp_tr_t1500, observed).
narrative_ontology:measurement(mc39_liberal_dp_tr_t1689, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1689, 0.14).
narrative_ontology:measurement_basis(mc39_liberal_dp_tr_t1689, observed).
narrative_ontology:measurement(mc39_liberal_dp_tr_t1850, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1850, 0.12).
narrative_ontology:measurement_basis(mc39_liberal_dp_tr_t1850, observed).
narrative_ontology:measurement(mc39_liberal_dp_tr_t1950, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1950, 0.16).
narrative_ontology:measurement_basis(mc39_liberal_dp_tr_t1950, observed).
narrative_ontology:measurement(mc39_liberal_dp_tr_t2026, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 2026, 0.22).
narrative_ontology:measurement_basis(mc39_liberal_dp_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(mc39_liberal_dp_be_t1215, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1215, 0.62).
narrative_ontology:measurement_basis(mc39_liberal_dp_be_t1215, observed).
narrative_ontology:measurement(mc39_liberal_dp_be_t1500, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1500, 0.48).
narrative_ontology:measurement_basis(mc39_liberal_dp_be_t1500, observed).
narrative_ontology:measurement(mc39_liberal_dp_be_t1689, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1689, 0.66).
narrative_ontology:measurement_basis(mc39_liberal_dp_be_t1689, observed).
narrative_ontology:measurement(mc39_liberal_dp_be_t1850, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1850, 0.72).
narrative_ontology:measurement_basis(mc39_liberal_dp_be_t1850, observed).
narrative_ontology:measurement(mc39_liberal_dp_be_t1950, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1950, 0.76).
narrative_ontology:measurement_basis(mc39_liberal_dp_be_t1950, observed).
narrative_ontology:measurement(mc39_liberal_dp_be_t2026, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 2026, 0.7).
narrative_ontology:measurement_basis(mc39_liberal_dp_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(mc39_liberal_dp_su_t1215, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1215, 0.58).
narrative_ontology:measurement_basis(mc39_liberal_dp_su_t1215, observed).
narrative_ontology:measurement(mc39_liberal_dp_su_t1500, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1500, 0.4).
narrative_ontology:measurement_basis(mc39_liberal_dp_su_t1500, observed).
narrative_ontology:measurement(mc39_liberal_dp_su_t1689, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1689, 0.52).
narrative_ontology:measurement_basis(mc39_liberal_dp_su_t1689, observed).
narrative_ontology:measurement(mc39_liberal_dp_su_t1850, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1850, 0.36).
narrative_ontology:measurement_basis(mc39_liberal_dp_su_t1850, observed).
narrative_ontology:measurement(mc39_liberal_dp_su_t1950, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1950, 0.34).
narrative_ontology:measurement_basis(mc39_liberal_dp_su_t1950, observed).
narrative_ontology:measurement(mc39_liberal_dp_su_t2026, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 2026, 0.42).
narrative_ontology:measurement_basis(mc39_liberal_dp_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__liberal_due_process_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39__feudal_prerogative_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39__originalist_limitation_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, habeas_corpus_writ_machinery).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, us_fourteenth_amendment_due_process).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, udhr_arbitrary_arrest_prohibition).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: 'what clause 39 establishes' is a colloquial label covering three structurally distinct claims that differ on the scope of the protected class and the referent of lawful process. Each reading gets its own story, its own epsilon, its own beneficiary/victim structure: this file authors the liberal reading (universal class, evolving referent, expansive constraint on executive discretion); the feudal and originalist siblings author narrower classes and fixed referents with correspondingly smaller epsilon. The siblings are linked here as family members; the liberal reading is downstream of the text's authority and upstream of modern due-process jurisprudence, which cites it as warrant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
