% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__originalist_reading, []).

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
 *   constraint_id: us_constitution_meaning__originalist_reading
 *   human_readable: Originalist Constitutional Constraint: Meaning Fixed at Ratification
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The originalist reading of the U.S. Constitution asserts that
 *   constitutional meaning is fixed at the moment of ratification (or
 *   amendment adoption) and cannot evolve. Judges applying originalism are
 *   bound to interpret the Constitution according to the public meaning of
 *   its text in 1788 (or the date of the relevant amendment), regardless of
 *   contemporary circumstances, values, or social change. This reading
 *   coordinates a counter-majoritarian judicial role—the judge is portrayed
 *   as passive, bound, and insulated from political pressure—while extracting
 *   substantial costs from rights claimants whose liberty interests lack
 *   historical precedent. The constraint is enforced through institutional
 *   pressure on judges (appointment criteria, peer evaluation, bar
 *   association status) and is reinforced by internalization: originalist
 *   judges often adopt the frame that they are merely following the law, not
 *   making choices. The extractiveness series tracks the rise of
 *   originalism's institutional dominance over the interval (1990s to
 *   present): initially a minority position among academics and judges,
 *   originalism has become dominant in federal judgeships and Supreme Court
 *   appointments, allowing it to suppress non-originalist interpretations
 *   with increasing force.
 *
 * KEY AGENTS:
 *   - originalist_judges_and_legal_scholars: Agenda-setters who interpret and enforce the constraint; derive legitimacy from the historical fidelity frame.
 *   - counter_majoritarian_constraint_advocates: Primary beneficiaries; their desired outcomes (conservative constitutional interpretation, narrow rights protection) are vindicated by originalism.
 *   - rights_claimants_lacking_historical_support: Victims; their liberty interests (reproductive freedom, same-sex marriage, equal protection of undocumented immigrants) lack 1788-era public meaning and are therefore suppressed.
 *   - living_constitutionalist_judges_and_scholars: Competing authority; their readings are suppressed by originalism's institutional dominance.
 *   - general_public_and_affected_citizens: Stakeholders experiencing the downstream effects of suppressed rights claims; diffuse position.
 *   - historical_record_interpreters: Analytical observers; their scholarship on founding-era meanings is the raw material originalism claims to use objectively but applies with discretion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, 0.68).
domain_priors:suppression_score(us_constitution_meaning__originalist_reading, 0.79).
domain_priors:theater_ratio(us_constitution_meaning__originalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__originalist_reading, "Originalist Constitutional Constraint: Meaning Fixed at Ratification").
narrative_ontology:topic_domain(us_constitution_meaning__originalist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__originalist_reading, '73d4659c-1bde-443b-b359-ee76c7100d0d').
narrative_ontology:cs_kernel_codification('73d4659c-1bde-443b-b359-ee76c7100d0d', fixed_text).
narrative_ontology:cs_authority_grounding('73d4659c-1bde-443b-b359-ee76c7100d0d', lineage).
narrative_ontology:cs_interpretation_layer_present('73d4659c-1bde-443b-b359-ee76c7100d0d').
narrative_ontology:cs_reading_relation('73d4659c-1bde-443b-b359-ee76c7100d0d', us_constitution_meaning__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('73d4659c-1bde-443b-b359-ee76c7100d0d', us_constitution_meaning__positivist_reading, influences).
narrative_ontology:cs_axiom('73d4659c-1bde-443b-b359-ee76c7100d0d', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('73d4659c-1bde-443b-b359-ee76c7100d0d', meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('73d4659c-1bde-443b-b359-ee76c7100d0d', foundational, judges_bound_by_historical_public_meaning).
narrative_ontology:cs_axiom_status(judges_bound_by_historical_public_meaning, holdable).
narrative_ontology:cs_axiom_grounding('73d4659c-1bde-443b-b359-ee76c7100d0d', judges_bound_by_historical_public_meaning, deontological).
narrative_ontology:cs_reference_frame('73d4659c-1bde-443b-b359-ee76c7100d0d', constitutional_meaning_as_fixed_historical_fact).
narrative_ontology:cs_drift_state('73d4659c-1bde-443b-b359-ee76c7100d0d', contemporary_post_2001_judicial_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('73d4659c-1bde-443b-b359-ee76c7100d0d', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__originalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, rights_claimants_lacking_historical_support).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, originalist_judges).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, general_public_and_affected_citizens).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, general_public_and_affected_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce the constraint through their judicial decisions. They frame originalism as objective fidelity to historical meaning, justifying their role as passive reading of law rather than making choices. They benefit from the professional legitimacy and clarity the constraint provides, insulating them from charges of activism. They are partly constrained by the historical text (if the text is clear, they cannot easily depart) but retain substantial interpretive latitude in establishing what the historical meaning was. Their exit from originalism would require abandoning their professional identity and exposing themselves to criticism for activism.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, originalist_judges, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__originalist_reading, originalist_judges, beneficiary).

% Advocates (conservative legal scholars, Federalist Society members, policy organizations) who promote originalism because it produces outcomes they favor: narrow rights protection, deference to states and legislatures, limited judicial power. They have built institutional power (law schools, think tanks, judicial appointment networks) to promote originalism. They benefit from the constraint because it systematically favors their preferred constitutional vision. Their exit options are strong: they can shift to other legal theories or political strategies if originalism ceases to serve their interests.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates, beneficiary,
    powerful, generational, arbitrage, national).

% Individuals and groups whose liberty interests or equal protection claims lack clear 1788-era historical precedent: reproductive freedom, LGBTQ+ rights, rights of undocumented immigrants, expansive free speech claims. They are suppressed by originalism because judges constrained by historical meaning will reject their claims. Their exit options are severely constrained: they cannot exit the need for constitutional protection, and they cannot change the historical record. Their only remedy is to pursue constitutional amendment (prohibitively difficult) or to wait for a change in judicial doctrine. Identity-locked: their claims are constitutive of their identity and status as citizens, not negotiable choices.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, rights_claimants_lacking_historical_support, payer,
    powerless, biographical, identity_locked, national).

% Judges who adopt living-constitutionalist readings but are suppressed by originalism's institutional dominance. They are excluded from agenda-setting as originalist judges rise to prominence in federal appointments. Their interpretive authority is delegitimized and their decisions are reversed or overruled by originalist appellate judges. They are trapped in the judiciary: they cannot easily exit their profession, but their preferred interpretation is institutionally suppressed. They would object to originalism if their voices were heard, but originalism's enforcement mechanisms (appointment criteria favoring originalists) prevent their ascendance.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, living_constitutionalist_judges, excluded,
    institutional, generational, trapped, national).

% Academics who develop and defend originalist theory, publish in law reviews, influence judicial appointments through scholarly networks and litigation. They set the intellectual terms for the constraint and provide the historical research and argumentation judges use to justify originalist decisions. They benefit from the constraint's institutional prominence: their scholarly work is cited by courts, their students enter judgeships, their ideas shape law. Their exit options are strong: they can shift to other scholarly projects or legal theories if originalism loses institutional support.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, originalist_legal_scholars, agenda_setter,
    powerful, generational, mobile, national).

% Citizens whose interests are affected by constitutional interpretation. Some benefit from originalism (those whose preferred constitutional vision aligns with historical meaning), and others pay the cost (those seeking rights protection for interests not contemplated in 1788). They are diffusely positioned: no individual has strong power, and their exit options are limited to political mobilization for constitutional amendment or appointment of different judges. They experience the constraint's effects through judicial decisions that suppress or protect their interests.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, general_public_and_affected_citizens, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__originalist_reading, general_public_and_affected_citizens, payer).

% Historians, legal historians, and documentary archivists who study and interpret 18th-century public meaning. Originalism claims to defer to their findings, but in practice their work is selectively used and interpreted by judges to reach preferred outcomes. They occupy an analytical seat: they do not directly benefit or pay, but their work provides the epistemic foundation for the constraint. They can observe the gap between their evidence and originalist judicial use of that evidence, but they lack power to enforce accurate representation.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, historical_record_interpreters, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates).
narrative_ontology:fixing_cost_class(us_constitution_meaning__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a principled constraint on judicial interpretation: judges cannot simply impose their policy preferences on constitutional meaning. Coordinates the rule-of-law function by binding judges to fixed law rather than allowing them to invent new meanings to suit contemporary politics or personal conviction.
% TRANSFER_FUNCTION: Transfers interpretive authority from individual judges' preferences to the historical public meaning of the constitutional text. Moves the cost of constitutional adaptation from judicial decision-making to the amendment process, requiring supermajority agreement to change constitutional meaning rather than permitting incremental judicial change.
% ABSENT_VOICES: Living-constitutionalist judges and scholars would object if present, arguing that historical meaning is ambiguous and application must evolve with circumstances. Rights claimants whose interests lack historical precedent would object, arguing that their fundamental interests should not depend on 18th-century foresight. Positivist legal theorists would object that originalism improperly grounds constitutional authority in external moral facts about public meaning rather than in institutional procedures. These voices are excluded by originalism's institutional dominance: they lack appointment power and their scholarly challenges are not integrated into the bench.
% DISAPPEARANCE_RATIONALE: If originalism vanished overnight—if judges abandoned historical-meaning constraint and adopted discretionary interpretation—constitutional law would reorganize substantially. Rights claims currently suppressed (reproductive freedom, LGBTQ+ rights, expansive equal protection) would likely be vindicated by judicial discretion. The federal-state balance would shift, as judges would expand federal power to address contemporary problems. Legislative deference would collapse, as judges would no longer bind themselves to historical limits on judicial power. The institutional role of the judge would become explicitly political. The world would rearrange because originalism is not a natural law but a constructed institutional constraint that reorganizes the distribution of interpretive authority.
% FOUNDING_PROBLEM: Judges in the late 20th century were accused of imposing their policy preferences on the Constitution—inventing rights (abortion, privacy) that the text did not support and expanding federal power beyond the Constitution's historical scope. The concern was that the Constitution had become a blank check for judicial activism. Originalism was developed as a response: if judges bound themselves to historical public meaning, they could not invent new rights or constitutional powers.
% FOUNDING_PROBLEM_CORROBORATION: Conservative legal scholars and originalist judges assert the founding problem is still live—judges remain tempted to activism and originalism is necessary restraint. Liberal legal scholars and living-constitutionalist judges assert the founding problem is substantially solved—contemporary judges are politically sophisticated, institutional norms restrain naked activism, and the real problem is that originalism masks policy choices under the cover of historical fidelity. The judicial record shows mixed evidence: some judges do engage in selective originalism to reach preferred outcomes (supporting the liberal critique), while others apply originalism even to reach personally disfavored results (supporting the conservative claim). Historical analysis suggests the founding problem was partly real (judicial discretion does exist) but originalism solves it by trading visible activism for hidden discretion (historical interpretation), not by eliminating discretion. No external, non-partisan authority has definitively resolved whether the founding problem persists.
narrative_ontology:disappearance_verdict(us_constitution_meaning__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_meaning__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__originalist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.31 to 0.68 over the interval because originalism's institutional power grew: it moved from an academic movement to a dominant position in federal judiciary and Supreme Court appointments (particularly from 2001 onward with the Bush administration, accelerating under Trump). High extractiveness (0.68 endpoint) reflects that the constraint now governs the interpretation of fundamental rights: judges constrained by originalism will reject rights claims that lack 1788-era support, no matter how urgent contemporary circumstances or majoritarian sentiment. Suppression is high (0.79) because originalism is enforced not just by external institutional mechanisms but by internalization—judges come to believe they are simply reading the law, not making choices. This self-enforcing character makes it hard to exit: a judge cannot simply 'choose' to ignore historical text. Theater ratio rises from 0.18 to 0.42 because an increasing share of originalist judging involves interpretation-within-discretion (how much historical evidence suffices, which sources count, how tightly to bind application to historical meaning) that appears to be mechanical but preserves substantial judicial latitude. The constraint coordinates a genuine judicial role (judges must not simply impose their policy preferences) while extracting from those whose interests fall outside the historical frame. Measured suppression (0.79) assumes originalism has become internalized and is actively enforced through appointment criteria; it is not merely a scholarly position but an institutional discipline.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist-judge and counter-majoritarian-advocate seats, the constraint appears as a principled, restraining force that prevents judicial overreach and keeps the Constitution fixed as written. From the rights-claimant seat, the same structure appears as pure extraction: a rule that says 'your interests do not count because they were not contemplated in 1788' suppresses legitimate claims without justifying why current circumstances should be irrelevant to constitutional meaning. From the living-constitutionalist judge seat, originalism is perceived as a false constraint that manufactures neutrality while achieving conservative outcomes. The engine computes these divergences from the structural data: originalist judges (powerful, institutional, low exit) sit near the beneficiary end; rights claimants (powerless to moderate, identity-locked in their needs, high cost of exit) sit near the target end.
 *
 * DIRECTIONALITY LOGIC:
 *   Counter-majoritarian constraint advocates (beneficiaries) benefit because originalism is their preferred judicial posture—it produces outcomes they favor and gives them institutional leverage. Their directionality is near 0.0 (full beneficiary): originalism vindicates their world-view and constrains judges in the direction they desire. Rights claimants lacking historical support (victims) bear the cost: their claims are suppressed, and they have no remedy except to change the Constitution itself (prohibitive exit). Their directionality is near 1.0 (full target): originalism systematically disadvantages them. Originalist judges sit between: they benefit from the apparent neutrality and constraint (reduced exposure to charges of activism) but also bear costs (forced to decide cases against their policy preferences when the historical text is clear). Their directionality is approximately 0.4-0.5 (moderate)—they are partly beneficiaries (in terms of professional legitimacy) and partly constrained (in terms of freedom of action). Living-constitutionalist scholars are excluded from the agenda-setting function by originalism's institutional dominance, giving them high d (near target) but their power level is analytical, so their effective extraction is lower.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of originalism is 'how to prevent judges from imposing their policy preferences on the Constitution.' This problem is contested: originalists assert it is still live (judges remain tempted to activism), while critics assert it is largely dead (contemporary judges are politically sophisticated and institutional safeguards exist). The constraint persists because originalist judges genuinely believe in the principle, and originalist scholars have built institutional power around it. However, the mandatrophy test (founding_problem_status x disappearance_verdict) flags an ambiguity: if the founding problem is substantially solved (judges are already politically accountable and institutional norms restrain activism), then originalism's persistence is partly inertia—it becomes a piton, not a rope. The measured theater_ratio (rising to 0.42) suggests growing performative character: originalist judges engage in increasingly complex historical interpretation that preserves discretion while maintaining the appearance of constraint. This supports a zombie classification: the founding problem (judicial activism) is solved or contested, but the constraint persists to serve the extracted interest (suppression of disfavored rights claims). The measurement series tracks this drift: base_extractiveness rises while theater_ratio also rises, indicating the coordination function (restraint on judges) is being displaced by the extraction function (suppression of rights claims).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_record_sufficiency,
    'How much 18th-century evidence suffices to establish ''public meaning'' at ratification? What counts as reliable evidence—elite intent, broad ratifier understanding, common usage, founding-era dictionaries?',
    'Comparative study of how originalist judges actually resolve ambiguous historical records; meta-analysis of dissents citing same-period sources to reach opposite conclusions.',
    'If the evidentiary standard is loose, originalism permits substantial judicial discretion under historical cover (tangled_rope with high theater). If strict, it genuinely constrains outcomes (reduces theater). The measured suppression (0.79) assumes moderate discretion—judges suppress non-historical outcomes but retain interpretive latitude within the historical record.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_record_sufficiency, empirical, 'Ambiguity and discretion in establishing 18th-century public meaning.').

omega_variable(
    living_vs_originalist_logical_space,
    'Do originalist and living-constitutionalist readings occupy a shared normative framework that permits simultaneous adoption, or does endorsing one reading logically foreclose the other?',
    'Examination of whether a judge could assert ''meaning is fixed at ratification AND meaning adapts to modern circumstances'' without contradiction. Test whether hybrid positions (living originalism, originalism with application flexibility) collapse into one reading or sustain coherence.',
    'If logically foreclosed: relation = ''forecloses''. If both remain live positions without contradiction: relation = ''coexists_with''. Current jurisprudence suggests coexistence (different judges adopt each) despite apparent tension, suggesting ''coexists_with'' is accurate—but the question remains open.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(living_vs_originalist_logical_space, conceptual, 'Logical compatibility of originalist and living-constitutionalist axioms.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the suppression of non-originalist outcomes (measured 0.79) primarily structural (judges face career/legitimacy penalties for departing originalism) or internalized (judges genuinely believe historical fidelity is binding)?',
    'Post-suppression trajectory: interview judges who leave the bench about whether originalist constraints persist when career penalties vanish. Examine whether originalist judges'' private writings diverge from opinions.',
    'If internalized, the constraint''s effective suppression is self-reinforcing and durable; if structural, removal of enforcement (e.g., appointment of non-originalist judges) would dissolve it quickly. High internalization would indicate the constraint has become constitutive of judicial identity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Internalization of originalist constraint on judicial reasoning.').

omega_variable(
    false_summit_naturalization,
    'Is originalism a natural law inherent in constitutional structure, or a constructed institutional reading that benefits counter-majoritarian constraint advocates by making their preferred judicial role appear inevitable and apolitical?',
    'Historical analysis: did originalism emerge as a neutral epistemic stance, or as a strategic posture adopted by conservative legal movements in the 1980s onward to resist expanding rights claims? Does the reading''s authority ground in logical necessity or in institutional power?',
    'If natural: originalism is a mountain, and the beneficiary declaration is FSM-triggered. If constructed: it is a tangled_rope in which one beneficiary (counter-majoritarian advocates) coordinates the constraint and another set (rights claimants) pays the cost. The measurement assumes the latter; this omega names the tension.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether originalism is a natural epistemic constraint or a constructed reading that naturalizes itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__originalist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_meaning__originalist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(us_c_tr_t4, us_constitution_meaning__originalist_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(us_c_tr_t8, us_constitution_meaning__originalist_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(us_c_tr_t12, us_constitution_meaning__originalist_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(us_c_tr_t16, us_constitution_meaning__originalist_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_meaning__originalist_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_meaning__originalist_reading, base_extractiveness, 0, 0.31).
narrative_ontology:measurement(us_c_be_t4, us_constitution_meaning__originalist_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(us_c_be_t8, us_constitution_meaning__originalist_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(us_c_be_t12, us_constitution_meaning__originalist_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(us_c_be_t16, us_constitution_meaning__originalist_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(us_c_be_t20, us_constitution_meaning__originalist_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_meaning__originalist_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(us_c_su_t4, us_constitution_meaning__originalist_reading, suppression_requirement, 4, 0.68).
narrative_ontology:measurement(us_c_su_t8, us_constitution_meaning__originalist_reading, suppression_requirement, 8, 0.73).
narrative_ontology:measurement(us_c_su_t12, us_constitution_meaning__originalist_reading, suppression_requirement, 12, 0.76).
narrative_ontology:measurement(us_c_su_t16, us_constitution_meaning__originalist_reading, suppression_requirement, 16, 0.78).
narrative_ontology:measurement(us_c_su_t20, us_constitution_meaning__originalist_reading, suppression_requirement, 20, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_meaning__originalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__positivist_reading).

% DUAL FORMULATION NOTE:
% The U.S. Constitution's meaning is a contested kernel with three structurally distinct readings. The originalist reading (this story) fixes meaning at ratification and suppresses non-historical claims (high extraction). The living-constitutionalist reading (separate story) allows meaning to evolve with circumstances and suppresses originalist rigidity (different ε, different victims). The positivist reading (separate story) grounds constitutional authority in institutional enactment and suppresses both historical meaning and evolutionary claims. Each reading instantiates a different constraint with different beneficiary/victim structures. They are linked via network.affects_constraints because each reading's institutional dominance constrains the others' application—originalism's rise in the judiciary suppresses living-constitutionalist outcomes; living constitutionalism's persistence in academia suppresses originalist authority claims; positivism's formal procedure-focus suppresses both meaning-based readings. The ε-invariance principle requires decomposition: measuring the 'constraint' with one reading produces a different ε than measuring it with another. Each reading is a separate story with its own ε, its own classification, its own stakeholders.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
