% ============================================================================
% CONSTRAINT STORY: later_amendment_eras__structural_housekeeping_amendments
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_later_amendment_eras__structural_housekeeping_amendments, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: later_amendment_eras__structural_housekeeping_amendments
 *   human_readable: Constitutional Housekeeping Amendments: Suppression of Electoral and Institutional Mechanical Failures
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The structural housekeeping amendments form a quieter constitutional
 *   family addressing mechanical failures in electoral mechanics,
 *   institutional sessions, and compensation mechanisms rather than expanding
 *   franchise rights (civil rights era) or restructuring the taxation and
 *   representation systems (progressive era). These amendments include the
 *   12th (contingent election procedure after Jefferson-Burr tie), 20th (lame
 *   duck session elimination), 22nd (presidential term limits), 23rd (DC
 *   presidential vote), 24th (poll tax abolition as mechanical failure of
 *   access, not primarily franchise expansion), and 27th (congressional pay
 *   delay mechanism). Each addresses a specific mechanical failure:
 *   contingent elections produce unclear succession; lame duck sessions
 *   create unnecessary delay and political chaos; indefinite presidencies
 *   accumulate power beyond original design; DC exclusion from presidential
 *   voting creates anomalous non-representation; poll taxes gate franchise on
 *   wealth rather than citizenship; Congress voting its own immediate pay
 *   raises creates self-dealing. The reading frames these as suppression of
 *   identified failures — the constraint is the amendment process itself,
 *   which has high procedural cost (super-majority, multi-year ratification)
 *   but produces remediable institutional regularization. The core
 *   beneficiary is institutional regularity and predictability; the primary
 *   victim set is incumbents who benefited from the old mechanical glitches.
 *
 * KEY AGENTS:
 *   - Contingently Disenfranchised (powerless/trapped) — DC residents, eighteen-year-olds, poll-tax-barred voters experiencing mechanical exclusion with no procedural exit
 *   - Reform Coalition (moderate/constrained) — state legislatures, youth suffrage organizers, poll tax abolitionists pushing for amendment across super-majority barriers
 *   - Beneficiary Incumbents (institutional/arbitrage) — congressional incumbents and state legislatures advantaged by existing mechanical glitches; experience amendment as extraction of prior advantages
 *   - Amendment Machine (organized/constrained) — the deliberate constitutional amendment process; has agency and visible completion conditions (sunset after ratification)
 *   - Lame Duck Session (institutional/arbitrage) — vestigial institutional form persisting after the 20th Amendment reduced its power; maintains performative existence
 *   - Congressional Pay Authority (powerful/constrained) — Congress as compensation-setter subject to 27th Amendment delay mechanism; constrained but still extractive
 *   - Analytical Observer (analytical/analytical) — risks naturalizing contingent 18th-19th century design choices as immutable features of electoral systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(later_amendment_eras__structural_housekeeping_amendments, 0.38).
domain_priors:suppression_score(later_amendment_eras__structural_housekeeping_amendments, 0.42).
domain_priors:theater_ratio(later_amendment_eras__structural_housekeeping_amendments, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(later_amendment_eras__structural_housekeeping_amendments, extractiveness, 0.38).
narrative_ontology:constraint_metric(later_amendment_eras__structural_housekeeping_amendments, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(later_amendment_eras__structural_housekeeping_amendments, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(later_amendment_eras__structural_housekeeping_amendments, tangled_rope).
narrative_ontology:human_readable(later_amendment_eras__structural_housekeeping_amendments, "Constitutional Housekeeping Amendments: Suppression of Electoral and Institutional Mechanical Failures").
narrative_ontology:topic_domain(later_amendment_eras__structural_housekeeping_amendments, "political/legal/constitutional").

domain_priors:requires_active_enforcement(later_amendment_eras__structural_housekeeping_amendments).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(later_amendment_eras__structural_housekeeping_amendments, '7f87d824-a893-4c1a-a954-245c8b8c82dc').
narrative_ontology:cs_kernel_codification('7f87d824-a893-4c1a-a954-245c8b8c82dc', fixed_text).
narrative_ontology:cs_authority_grounding('7f87d824-a893-4c1a-a954-245c8b8c82dc', lineage).
narrative_ontology:cs_interpretation_layer_present('7f87d824-a893-4c1a-a954-245c8b8c82dc').
narrative_ontology:cs_reading_relation('7f87d824-a893-4c1a-a954-245c8b8c82dc', later_amendment_eras__reconstruction_amendments, coexists_with).
narrative_ontology:cs_reading_relation('7f87d824-a893-4c1a-a954-245c8b8c82dc', later_amendment_eras__progressive_era_amendments, coexists_with).
narrative_ontology:cs_reading_relation('7f87d824-a893-4c1a-a954-245c8b8c82dc', later_amendment_eras__civil_rights_era_amendments, coexists_with).
narrative_ontology:cs_axiom('7f87d824-a893-4c1a-a954-245c8b8c82dc', foundational, mechanical_failures_are_remediable).
narrative_ontology:cs_axiom_status(mechanical_failures_are_remediable, holdable).
narrative_ontology:cs_axiom_grounding('7f87d824-a893-4c1a-a954-245c8b8c82dc', mechanical_failures_are_remediable, empirically_contingent).
narrative_ontology:cs_axiom('7f87d824-a893-4c1a-a954-245c8b8c82dc', foundational, institutional_regularity_as_primary_beneficiary).
narrative_ontology:cs_axiom_status(institutional_regularity_as_primary_beneficiary, holdable).
narrative_ontology:cs_axiom_grounding('7f87d824-a893-4c1a-a954-245c8b8c82dc', institutional_regularity_as_primary_beneficiary, conventional).
narrative_ontology:cs_reference_frame('7f87d824-a893-4c1a-a954-245c8b8c82dc', constitutional_amendment_as_mechanical_repair).
narrative_ontology:cs_drift_state('7f87d824-a893-4c1a-a954-245c8b8c82dc', contemporary_constitutional_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7f87d824-a893-4c1a-a954-245c8b8c82dc', '2026-02-26T14:23:45Z').
narrative_ontology:cs_kernel_id(later_amendment_eras__structural_housekeeping_amendments, later_amendment_eras).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(later_amendment_eras__structural_housekeeping_amendments, institutional_regularity).
narrative_ontology:constraint_beneficiary(later_amendment_eras__structural_housekeeping_amendments, electoral_predictability).
narrative_ontology:constraint_victim(later_amendment_eras__structural_housekeeping_amendments, incumbents_advantaged_by_old_mechanics).
narrative_ontology:constraint_victim(later_amendment_eras__structural_housekeeping_amendments, delayed_pay_adjustment_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTINGENTLY DISENFRANCHISED (SNARE) — Citizens locked out by mechanical failures in the original electoral design (DC residents without presidential vote; eighteen-year-olds prohibited from voting; those unable to pay poll taxes). Cannot exit the constraint until constitutional amendment removes the mechanism. Experiences maximum extraction: excluded from franchise participation with no procedural remedy short of amendment. The constraint's suppression falls entirely on this agent — no exit options exist within the operating system.
constraint_indexing:constraint_classification(later_amendment_eras__structural_housekeeping_amendments, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM COALITION (TANGLED ROPE) — State legislatures and organized voter blocs pushing for amendment (youth suffrage organizers, DC representation advocates, poll tax abolitionists). Constrained by the super-majority requirement (2/3 both houses, 3/4 states) but experience genuine coordination benefit: the amendment process itself enables collective problem-solving around identified mechanical failures. Some extraction embedded in timing delays and compromise sacrifices, but also real coordination function. Experiences moderate-high suppression from the amendment process's high procedural bar.
constraint_indexing:constraint_classification(later_amendment_eras__structural_housekeeping_amendments, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BENEFICIARY INCUMBENTS — Congressional incumbents, state legislatures advantaged by existing electoral mechanics (guaranteed DC absence from presidential vote, predictable lame duck power, presidential term reset cycles, controlled pay through delegation). Experience the constraint as erosion of advantage: the amendment process repairs mechanical failures, but the repair flow extracts from those who benefited from the glitches. Net beneficiary of the old system, victim of the new. The institutional perspective sees amendment as coordination with real extraction cost: lose predictable advantages to gain stable rules.
constraint_indexing:constraint_classification(later_amendment_eras__structural_housekeeping_amendments, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DELIBERATE AMENDMENT MACHINE (SCAFFOLD) — The constitutional amendment process itself: slow, deliberate, requiring deliberation across states and generations. Organized agents (reform networks, state legislatures, constitutional scholars) experience this as a temporary friction mechanism with a sunset property: the amendment, once ratified, becomes permanent constitutional law. The scaffold's sunset is the moment of final ratification — the problem-solving process (suppression, negotiation, compromise) ends, and the solution becomes incumbent institutional design. Low effective extraction because the process has visible agency and clear completion conditions.
constraint_indexing:constraint_classification(later_amendment_eras__structural_housekeeping_amendments, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LAME DUCK SESSION (PITON) — The 20th Amendment shortened the lame duck period (moving inauguration from March to January). The constraint here is institutional inertia around the lame duck session as a lingering performance: the session still occurs in the gap between election and new term, but its power is largely theater (outgoing representatives who will not govern again, limited legislative capacity). The amendment recognized the mechanical failure (unnecessary delay, predictable political chaos) and suppressed it partially, but the lame duck session persists as vestigial performance — representatives gather, conduct ceremonial business, but lack the authority or political capital of a governing body. Theater ratio high because the form continues despite reduced function.
constraint_indexing:constraint_classification(later_amendment_eras__structural_housekeeping_amendments, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: CONGRESSIONAL PAY SELF-DEALING (TANGLED ROPE) — The 27th Amendment prohibits Congress from raising its own pay in the current session (raises take effect only after the next election). This constraint involves both coordination (establishing rational salary-setting through delayed-effect mechanism) and extraction (Congress still controls its own compensation, extracting delayed but guaranteed increases). The mechanism suppresses the most egregious self-dealing (immediate raises) but not the underlying extraction (Congress still votes its own pay). Powerful agent (Congress) is constrained by the delayed-effect rule but also benefits from having a constitutional basis for compensation increases. Mixed coordination-extraction hybrid: coordinates rational pay across electoral cycles while still enabling extraction through delay.
constraint_indexing:constraint_classification(later_amendment_eras__structural_housekeeping_amendments, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, mechanical failures in electoral design are inevitable features of any complex institutional system: contingent election outcomes, succession ambiguities, pay-setting loops. This perspective sees the housekeeping amendments as suppressing immutable structural limits of democratic governance. However, the structural data contradicts the mountain classification — the engine will identify this as a false summit: the mechanical failures are contingent design choices of the 18th-19th century, not natural laws. The amendments prove the failures are remediable, not inherent.
constraint_indexing:constraint_classification(later_amendment_eras__structural_housekeeping_amendments, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(later_amendment_eras__structural_housekeeping_amendments_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(later_amendment_eras__structural_housekeeping_amendments, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(later_amendment_eras__structural_housekeeping_amendments, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(later_amendment_eras__structural_housekeeping_amendments, TR),
    TR >= 0.70.

:- end_tests(later_amendment_eras__structural_housekeeping_amendments_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The housekeeping amendments suppress identified mechanical failures, reducing extraction over time. However, some extraction persists: the amendment process itself involves delay and compromise (reform coalition bears cost); congressional pay rises remain delayed-but-guaranteed (extraction deferred, not eliminated); the lame duck session still occurs in diminished form (theater persists despite functional suppression). The trajectory shows rising extractiveness over the interval because later amendments (22nd on term limits, 27th on congressional pay) address more contestable institutional design choices rather than clear mechanical flaws, embedding more partisan extraction into the amendment itself. Suppression (0.42): Moderate. The amendment process creates high procedural barriers (super-majority requirement, state ratification, multi-year timelines) that suppress reform attempts. However, the suppression is not total — the amendments eventually pass, demonstrating that the barrier is high but not insurmountable. Suppression requirement decreases over the interval as political consensus builds around the mechanical failures and amendment becomes easier (earlier amendments more contested, later amendments more smoothly ratified). Theater ratio (0.55): Moderate. The amendments address real mechanical failures (not purely performative), but the amendment process itself involves substantial performative content: legislative debate, state-by-state ratification ceremonies, symbolic affirmation of constitutional amendment as ritual. The theater increases over the interval as the mechanical justification becomes more attenuated (congressional term limits in 22nd Amendment are more politically contested than DC voting rights in 23rd), embedding more theater into the ratification process.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates significant perspectival divergence. The contingently disenfranchised see pure extraction (Snare) — they are excluded with no exit. The reform coalition sees coordination with high procedural cost (Tangled Rope) — the amendment process enables collective problem-solving but requires super-majority compromise. The beneficiary incumbents see extraction FROM them (their advantages being suppressed) — they experience the amendment as Tangled Rope where they are now the victim of the coordination mechanism. The amendment machine itself sees the constraint as a temporary scaffold with a clear sunset (ratification completes the process). The lame duck session sees its own degradation (Piton) — still existing but largely performative. Congressional pay sees coordination with embedded delayed extraction (Tangled Rope) — the mechanism is rational but still self-dealing. The analytical observer risks seeing mechanical failures as natural features of electoral systems (Mountain) — but the historical record shows they are remediable contingencies. The perspectival gap reveals that 'mechanical failure' is observer-dependent: what the reformer sees as a failure, the beneficiary incumbent sees as their advantage; what the amendment machine sees as a solvable coordination problem, the contingently disenfranchised see as pure exclusion.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) vary across perspectives based on structural position relative to the amendment constraint. The contingently disenfranchised occupy d ≈ 0.95 (full target, trapped, powerless) — all extraction, no arbitrage. The reform coalition occupies d ≈ 0.55 (organized victim of high procedural cost, but with agency through coalition) — substantial extraction but with collective power. The beneficiary incumbents occupy d ≈ 0.25 (formerly advantaged, now losing prior benefits) — they perceive extraction FROM them as the amendment suppresses their mechanical advantages. The amendment machine occupies d ≈ 0.50 (symmetric: both enables and constrains) — neither pure beneficiary nor pure victim. The lame duck session occupies d ≈ 0.30 (institutional beneficiary of vestigial form) — still receives funding and authority despite reduced function. Congressional pay occupies d ≈ 0.40 (powerful agent constrained but still extractive through delay guarantee) — constrained extraction. The analytical observer occupies d ≈ 0.72 (analytical position, neither target nor beneficiary, observes the full structure) — canonical analytical directionality. These d values feed the sigmoid f(d) to produce the effective extractiveness (χ) each perspective experiences.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanical_failure_vs_designed_feature,
    'Are the ''mechanical failures'' addressed by housekeeping amendments genuine design flaws, or were they intentional features that became obsolete as political context shifted?',
    'Historical analysis of the Founding-era intent for DC voting rights, term limits, and pay mechanisms; examination of whether the original designers explicitly chose to exclude these or left them ambiguous',
    'If genuine flaws: the amendments suppress real structural failures (Tangled Rope analysis holds). If designed features: the amendments represent a rewriting of foundational choices (shifts toward Snare for beneficiary incumbents, toward contested kernel reading entirely).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanical_failure_vs_designed_feature, empirical, 'Whether mechanical failures were design flaws or intentional features').

omega_variable(
    extractiveness_of_delayed_pay,
    'Does the 27th Amendment''s delayed-effect mechanism actually prevent self-dealing extraction, or merely delay it? Is the guaranteed increase (just deferred) a form of extraction hidden by the temporal offset?',
    'Comparison of congressional compensation growth before and after the 27th Amendment; analysis of whether delayed increases provide sufficient incentive structure vs. immediate extraction',
    'If delay prevents extraction: the constraint is pure coordination (Rope classification). If delay merely conceals ongoing extraction: the constraint is Tangled Rope (coordination shell around delayed extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extractiveness_of_delayed_pay, empirical, 'Whether 27th Amendment prevents or merely delays compensation extraction').

omega_variable(
    amendment_ratification_as_coercion,
    'Does the super-majority requirement for constitutional amendment constitute a suppression mechanism (high procedural bar preventing reform) or a legitimate deliberation gate (ensuring broad legitimacy before institutional change)?',
    'Historical analysis of blocked amendments; comparison of amendment success rates across different eras; examination of whether the bar prevented beneficial reforms or protected against harmful ones',
    'If suppression mechanism: the reformers experience higher extraction than analysis suggests (push from Tangled Rope toward Snare). If deliberation gate: the suppression value is justified protection, not extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_ratification_as_coercion, conceptual, 'Super-majority requirement as suppression vs. deliberation').

omega_variable(
    reading_contest_foreclosure,
    'Does the structural_housekeeping_amendments reading foreclose, coexist with, or influence the reconstruction_amendments, civil_rights_era_amendments, and progressive_era_amendments readings?',
    'Logical analysis of whether affirming ''mechanical failures need suppression via housekeeping'' entails or contradicts affirming ''slavery must be abolished,'' ''suffrage must be extended,'' or ''representation must be direct.'' The readings operate on different parts of the constitutional system and address different types of constraint.',
    'If foreclosure: the readings cannot coexist in a single constitutional framework (logical inconsistency). If coexistence: multiple readings remain live across different parties'' interpretations. If influence: this reading creates structural pressure on siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Logical relationship between structural housekeeping and other amendment era readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(later_amendment_eras__structural_housekeeping_amendments, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(housekeep_theater_t0, later_amendment_eras__structural_housekeeping_amendments, theater_ratio, 0, 0.42).
narrative_ontology:measurement(housekeep_theater_t50, later_amendment_eras__structural_housekeeping_amendments, theater_ratio, 50, 0.55).
narrative_ontology:measurement(housekeep_theater_t100, later_amendment_eras__structural_housekeeping_amendments, theater_ratio, 100, 0.65).

% Extraction over time
narrative_ontology:measurement(housekeep_extract_t0, later_amendment_eras__structural_housekeeping_amendments, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(housekeep_extract_t50, later_amendment_eras__structural_housekeeping_amendments, base_extractiveness, 50, 0.33).
narrative_ontology:measurement(housekeep_extract_t100, later_amendment_eras__structural_housekeeping_amendments, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(housekeep_suppress_t0, later_amendment_eras__structural_housekeeping_amendments, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(housekeep_suppress_t50, later_amendment_eras__structural_housekeeping_amendments, suppression_requirement, 50, 0.52).
narrative_ontology:measurement(housekeep_suppress_t100, later_amendment_eras__structural_housekeeping_amendments, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(later_amendment_eras__structural_housekeeping_amendments, enforcement_mechanism).
narrative_ontology:affects_constraint(later_amendment_eras__structural_housekeeping_amendments, reconstruction_amendments).
narrative_ontology:affects_constraint(later_amendment_eras__structural_housekeeping_amendments, progressive_era_amendments).
narrative_ontology:affects_constraint(later_amendment_eras__structural_housekeeping_amendments, civil_rights_era_amendments).

% DUAL FORMULATION NOTE:
% The structural housekeeping amendments form a constraint family with other amendment era constraints. Each reading of the later_amendment_eras kernel has its own extractiveness value, beneficiary/victim structure, and theater signature reflecting the different types of constitutional work the amendments perform. Housekeeping amendments suppress mechanical failures (ε ≈ 0.38); Reconstruction reorders fundamental rights (ε varies by which particular Reconstruction amendment); Progressive era retools power structures (ε varies); Civil rights extends franchise (ε varies). These are not the same constraint viewed differently — they are structurally distinct constraints linked through their shared kernel (the Constitution as amendable institution).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(later_amendment_eras__structural_housekeeping_amendments, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
