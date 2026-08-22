% ============================================================================
% CONSTRAINT STORY: us_constitution_text__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__living_constitutionalist_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: us_constitution_text__living_constitutionalist_reading
 *   human_readable: Living Constitutionalist Reading of Constitutional Authority
 *   domain: constitutional_law/legal_philosophy
 *
 * SUMMARY:
 *   This story generates ONE reading of the contested US Constitution kernel
 *   — the living constitutionalist reading, which holds that constitutional
 *   principles must be applied and updated in light of contemporary
 *   circumstances rather than frozen at the moment of ratification. This is
 *   distinct from the originalist reading (fixed meaning at ratification) and
 *   the positivist reading (validity from enactment procedure alone), which
 *   are separate constraint stories, not alternative measurements of this
 *   one. Landmark applications include Brown v. Board (1954, equal protection
 *   reread against segregation's social meaning), Griswold/Roe-era privacy
 *   doctrine, and Obergefell v. Hodges (2015, marriage equality). Under this
 *   reading's own lights, the standing arrangement under contest is judicial
 *   exercise of adaptive interpretive authority as it actually operates today
 *   — moderately extractive of the democratic amendment channel, not extreme,
 *   because courts still operate within doctrinal constraints (precedent,
 *   textual anchors, multi-member deliberation) even while claiming latitude
 *   to update meaning.
 *
 * KEY AGENTS:
 *   - rights_claimants_in_changed_social_contexts: Primary beneficiary (moderate/constrained) — gains legal recognition unavailable under fixed-meaning readings
 *   - federal_judiciary_interpretive_authority: Agenda-setter and structural beneficiary (institutional/arbitrage) — administers and expands the interpretive latitude this reading authorizes
 *   - fixed_meaning_democratic_constraint_advocates: Primary payer (organized/constrained) — bears the cost of amendment-process bypass
 *   - state_legislatures: Excluded party (organized/trapped) — the designed Article V channel rendered moot for judicially-achieved changes
 *   - constitutional_law_scholars: Analytical observer — shapes which doctrinal versions of this reading gain traction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__living_constitutionalist_reading, 0.38).
domain_priors:suppression_score(us_constitution_text__living_constitutionalist_reading, 0.22).
domain_priors:theater_ratio(us_constitution_text__living_constitutionalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__living_constitutionalist_reading, rope).
narrative_ontology:human_readable(us_constitution_text__living_constitutionalist_reading, "Living Constitutionalist Reading of Constitutional Authority").
narrative_ontology:topic_domain(us_constitution_text__living_constitutionalist_reading, "constitutional_law/legal_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__living_constitutionalist_reading, '74039688-a956-4506-9621-e6b8f8df0352').
narrative_ontology:cs_kernel_codification('74039688-a956-4506-9621-e6b8f8df0352', fixed_text).
narrative_ontology:cs_authority_grounding('74039688-a956-4506-9621-e6b8f8df0352', lineage).
narrative_ontology:cs_interpretation_layer_present('74039688-a956-4506-9621-e6b8f8df0352').
narrative_ontology:cs_reading_relation('74039688-a956-4506-9621-e6b8f8df0352', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('74039688-a956-4506-9621-e6b8f8df0352', us_constitution_text__positivist_reading, influences).
narrative_ontology:cs_axiom('74039688-a956-4506-9621-e6b8f8df0352', foundational, constitutional_text_embodies_adaptable_principles).
narrative_ontology:cs_axiom_status(constitutional_text_embodies_adaptable_principles, holdable).
narrative_ontology:cs_axiom_grounding('74039688-a956-4506-9621-e6b8f8df0352', constitutional_text_embodies_adaptable_principles, conventional).
narrative_ontology:cs_axiom('74039688-a956-4506-9621-e6b8f8df0352', foundational, post_ratification_practice_and_social_change_carry_interpretive_authority).
narrative_ontology:cs_axiom_status(post_ratification_practice_and_social_change_carry_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('74039688-a956-4506-9621-e6b8f8df0352', post_ratification_practice_and_social_change_carry_interpretive_authority, instrumental).
narrative_ontology:cs_reference_frame('74039688-a956-4506-9621-e6b8f8df0352', textual_generality_requiring_ongoing_application).
narrative_ontology:cs_drift_state('74039688-a956-4506-9621-e6b8f8df0352', post_warren_court_rights_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('74039688-a956-4506-9621-e6b8f8df0352', '').
narrative_ontology:cs_kernel_id(us_constitution_text__living_constitutionalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, rights_claimants_in_changed_social_contexts).
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, federal_judiciary_interpretive_authority).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, fixed_meaning_democratic_constraint_advocates).
narrative_ontology:constraint_vindicates(us_constitution_text__living_constitutionalist_reading, constitutional_adaptability_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_text__living_constitutionalist_reading, evolving_standards_of_decency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals seeking recognition of rights (marriage equality, reproductive autonomy, privacy protections) not contemplated or intended by the ratifying generation. They rely on courts reading constitutional text as embodying broader principles that can be applied to circumstances the framers did not anticipate. Their claims succeed or fail depending on whether the judiciary accepts an adaptive reading; they have no alternative legal avenue if courts insist on fixed original meaning.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, rights_claimants_in_changed_social_contexts, beneficiary,
    moderate, generational, constrained, national).

% Article III courts, and especially the Supreme Court, exercise the interpretive discretion this reading authorizes. They determine which contemporary values and social changes count as constitutionally relevant, expanding their own institutional authority to update constitutional meaning without formal amendment. Their power grows in direct proportion to how much interpretive latitude this reading grants them.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, federal_judiciary_interpretive_authority, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__living_constitutionalist_reading, federal_judiciary_interpretive_authority, beneficiary).

% Citizens and political actors who believe constitutional change should occur only through the Article V amendment process, ratified by supermajorities of elected representatives and states. They experience judicial updating of constitutional meaning as an end-run around the deliberately difficult amendment process, effectively disenfranchising the democratic majorities whose consent Article V was designed to require. Their only recourse is the amendment process itself, appointment politics, or waiting for composition changes on the bench — all slow, uncertain, and asymmetric against a judiciary that need not wait for anyone's consent to interpret.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, fixed_meaning_democratic_constraint_advocates, payer,
    organized, generational, constrained, national).

% Bodies whose role in the Article V amendment process (ratifying constitutional amendments) is the designed democratic channel for constitutional change. When courts update meaning through interpretation rather than amendment, state legislatures are bypassed entirely — their constitutionally assigned role in changing fundamental law is rendered moot for the changes courts choose to make judicially.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, state_legislatures, excluded,
    organized, generational, trapped, national).

% Academic and judicial theorists who study and debate the legitimacy of adaptive interpretation, producing scholarship, judicial opinions, and doctrinal frameworks (rational basis, strict scrutiny, evolving standards of decency) that operationalize the living constitutionalist approach. They do not directly benefit or pay but shape which version of the reading gains institutional traction.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__living_constitutionalist_reading, federal_judiciary_interpretive_authority).
narrative_ontology:fixing_cost_class(us_constitution_text__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for constitutional principles to remain applicable as social conditions, technology, and moral understanding change over decades and centuries, without requiring the difficult and infrequent process of formal amendment for every adaptation.
% TRANSFER_FUNCTION: Moves interpretive authority over the meaning of fundamental law from the amendment process (Article V, requiring congressional supermajorities and state ratification) to the judiciary; moves the practical capacity to obtain new constitutional protections from legislative majorities to litigants who can construct persuasive doctrinal arguments before courts.
% ABSENT_VOICES: State legislatures and the ratifying public who would object that Article V's deliberately high bar for constitutional change is circumvented when courts achieve the same substantive result through interpretation; they are structurally absent from the judicial process that makes the change, having no vote or veto over a ruling the way they would over a formal amendment.
% DISAPPEARANCE_RATIONALE: If the living constitutionalist reading were unavailable and courts strictly confined themselves to original public meaning, contemporary rights recognized without textual analogue at ratification (same-sex marriage, many privacy and reproductive rights, incorporation-era expansions) would require formal Article V amendment or would revert to being contestable political questions decided by ordinary legislation — a substantial rearrangement of which branch and which majority controls constitutional change.
% FOUNDING_PROBLEM: The Constitution's text is terse and its ratifying generation could not anticipate technologies, social structures, and moral questions (electronic surveillance, same-sex relationships, in vitro fertilization, algorithmic decision-making) that later generations would need constitutional principles to address; a purely fixed-meaning approach risks either constitutional irrelevance or a cascade of impractical amendment attempts for every emerging question.
% FOUNDING_PROBLEM_CORROBORATION: Sitting justices across the ideological spectrum acknowledge some interpretive latitude is unavoidable given text's generality (even originalists like Scalia conceded 'living' application of general clauses to new facts). Originalist scholars and some political scientists outside the judiciary attest the founding problem is real but argue it has been resolved by the amendment process itself and that judicial adaptation instead substitutes judicial preference for democratic deliberation — a corroboration that comes from a rival reading's own camp, which is the best evidence available that the disagreement is genuine rather than manufactured by beneficiaries.
narrative_ontology:disappearance_verdict(us_constitution_text__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__living_constitutionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__living_constitutionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_text__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__living_constitutionalist_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__living_constitutionalist_reading_tests).
:- end_tests(us_constitution_text__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at 2024) and rising slowly: the reading transfers real decision-making power from the amendment process to courts, but this transfer is bounded by stare decisis, multi-justice deliberation, and the continuing need for textual anchoring — it is not unconstrained judicial fiat. Suppression is low (0.22): the reading does not suppress the originalist alternative from being argued, litigated, or adopted by differently-composed courts; both readings coexist in ongoing doctrinal contest, and the swing between them (Roe to Dobbs) demonstrates neither reading has captured the field permanently. Accessibility collapse is moderate-low (0.35): the fixed-meaning alternative remains fully articulable and periodically prevails, so alternatives have not collapsed. Resistance is comparatively high (0.68): originalist scholarship, dissenting opinions, and political mobilization around judicial appointments constitute sustained, well-organized resistance to this reading's expansions.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights claimants sit near the beneficiary end: the reading is the mechanism by which their claims can succeed at all in the absence of textual analogue or amendment. The federal judiciary is also a structural beneficiary — its interpretive latitude is itself the good the reading protects, independent of any particular substantive outcome, which is why judicial power expands under this reading regardless of ideological direction. Fixed-meaning-constraint advocates sit near the target end: their preferred check on constitutional change (Article V's supermajority requirement) is functionally bypassed whenever courts achieve substantive change through interpretation instead. State legislatures are excluded rather than coordinated — their designed constitutional role goes unused for judicially-achieved changes, which is a structural bypass rather than a benefit or an active extraction from them specifically.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (constitutional text's necessary generality outrunning the ratifying generation's specific foresight) remains genuinely live — new technologies and social arrangements continue to generate questions no ratification-era public could have anticipated. This is why founding_problem_status is 'contested' rather than 'dead': unlike a pure zombie mandate, the adaptation problem this reading addresses has not gone away. What is contested is not whether adaptation is needed but WHO should perform it — courts via interpretation, or the public via Article V. Classifying this as rope (rather than snare) reflects that genuine coordination function (keeping constitutional principles applicable across time) coexists with real transfer costs to the amendment-process constituency, without suppression of the rival channel rising to extraction-requires-coercion levels.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    living_constitutionalism_vs_originalism_kernel_contest,
    'Is the living constitutionalist reading a legitimate exercise of the judiciary''s interpretive function, or does it constitute an unauthorized amendment power exercised without Article V''s democratic supermajority requirement?',
    'No empirical resolution exists — this is a live jurisprudential and political dispute contested across generations of judicial appointments, constitutional scholarship, and popular constitutionalism movements. The sibling originalist_reading constraint story authors the opposing structural claim: that this reading''s beneficiary/victim structure is inverted (fixed-meaning advocates are the true beneficiaries of legitimate constitutional order; adaptive-reading claimants are illegitimately privileged).',
    'If the originalist critique is correct, this reading''s coordination function claim (keeping the Constitution applicable across time) is largely cover for judicial policy-making, and the true classification approaches tangled_rope or snare rather than rope. If the living constitutionalist defense is correct, the coordination function is genuine and the extraction from fixed-meaning advocates is the necessary cost of a functioning constitutional order across centuries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(living_constitutionalism_vs_originalism_kernel_contest, conceptual, 'The core kernel contest between this reading and the originalist sibling reading — routed here per Rule 2 rather than folded into this story''s classification.').

omega_variable(
    judicial_restraint_variability,
    'How much does the actual degree of extraction from the democratic amendment channel vary depending on which doctrinal test (rational basis vs. strict scrutiny vs. evolving standards of decency) a given court applies under this reading?',
    'Comparative analysis of case outcomes across doctrinal tests and across court compositions (Warren, Burger, Rehnquist, Roberts courts) to determine whether extraction is a stable property of the reading itself or highly composition-dependent.',
    'If extraction varies enormously by court composition, the reading''s ε is less a property of the interpretive theory than of who currently sits on the bench — suggesting the ''reading'' label may itself be doing less classificatory work than judicial personnel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_restraint_variability, empirical, 'Whether measured extraction is a stable property of the reading or an artifact of court composition.').

omega_variable(
    amendment_process_realism,
    'Is Article V''s formal amendment process a realistic alternative channel in the modern era (given supermajority requirements now nearly unattainable amid political polarization), or has it become effectively dead, making judicial interpretation the only functioning channel for constitutional adaptation regardless of this reading''s legitimacy?',
    'Historical frequency analysis of successful amendments post-1971 (none) versus successful judicially-recognized constitutional changes in the same period, weighed against political science literature on polarization''s effect on supermajority processes.',
    'If Article V is functionally dead, the ''victim'' framing of fixed-meaning-constraint advocates changes: they are not being bypassed from a live channel but are invoking a channel that no longer functions, which would lower the effective extraction this reading imposes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_process_realism, empirical, 'Whether the amendment process this reading is said to bypass remains a live alternative or has independently atrophied.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__living_constitutionalist_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1954, us_constitution_text__living_constitutionalist_reading, theater_ratio, 1954, 0.18).
narrative_ontology:measurement(us_c_tr_t1965, us_constitution_text__living_constitutionalist_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(us_c_tr_t1973, us_constitution_text__living_constitutionalist_reading, theater_ratio, 1973, 0.24).
narrative_ontology:measurement(us_c_tr_t1992, us_constitution_text__living_constitutionalist_reading, theater_ratio, 1992, 0.24).
narrative_ontology:measurement(us_c_tr_t2003, us_constitution_text__living_constitutionalist_reading, theater_ratio, 2003, 0.26).
narrative_ontology:measurement(us_c_tr_t2015, us_constitution_text__living_constitutionalist_reading, theater_ratio, 2015, 0.27).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_text__living_constitutionalist_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1954, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 1954, 0.22).
narrative_ontology:measurement(us_c_be_t1965, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 1965, 0.26).
narrative_ontology:measurement(us_c_be_t1973, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 1973, 0.3).
narrative_ontology:measurement(us_c_be_t1992, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 1992, 0.33).
narrative_ontology:measurement(us_c_be_t2003, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 2003, 0.34).
narrative_ontology:measurement(us_c_be_t2015, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 2015, 0.36).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 2024, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(us_constitution_text__living_constitutionalist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_text__living_constitutionalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__positivist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the us_constitution_text kernel (living_constitutionalist_reading, originalist_reading, positivist_reading). Each reading authors its own ε, beneficiary/victim structure, and claimed_type from its own interpretive premises; none average across the others. The living constitutionalist reading's beneficiary (rights claimants in changed contexts) is structurally inverted relative to the originalist reading's beneficiary (fixed-meaning democratic constraint), while the positivist reading brackets the moral-content question entirely and evaluates only enactment-procedure validity, making it more orthogonal than opposed to the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_text__living_constitutionalist_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
