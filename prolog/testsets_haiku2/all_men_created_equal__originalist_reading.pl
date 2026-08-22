% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__originalist_reading, []).

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
 *   constraint_id: all_men_created_equal__originalist_reading
 *   human_readable: Equality as Bounded by Founders' Intent (Originalist Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The originalist reading of 'all men are created equal' bounds the
 *   principle's scope to what the founders demonstrably intended: equal
 *   rights for free white propertied males. The reading uses historical
 *   fidelity as its legitimizing frame—the constraint persists because the
 *   founders' intent is treated as authoritatively dispositive over
 *   contemporary demands for expansion. This reading is one of three
 *   structurally distinct instantiations of the contested 'all men created
 *   equal' kernel. The originalist reading instantiates high extraction: it
 *   benefits the founding elite's descendants and propertied classes by
 *   protecting existing hierarchies from egalitarian challenge, while
 *   victimizing those the founders explicitly excluded. The constraint is
 *   claimed as tangled_rope (genuine coordination function—stable
 *   interpretation—paired with asymmetric extraction protecting beneficiary
 *   elites) and metrics are authored to reflect substantive enforcement
 *   requirements and rising theatrical justification over time as political
 *   pressure for expansion accumulates.
 *
 * KEY AGENTS:
 *   - Founding elite descendants: benefit from the interpretation; control the originalist frame
 *   - Propertied classes: benefit from property-protective doctrine; maintain political hegemony
 *   - Enslaved Africans, indigenous peoples, women, non-property-holders: payers/victims whose exclusion is justified by the constraint
 *   - Originalist interpretive community: agenda-setter; controls which historical sources are canonical
 *   - Universalist movements: excluded; their readings are treated as extra-constitutional
 *   - Textualist critics: excluded; they argue the contradiction itself is constitutional fact
 *   - Progressive judicial bloc: observer; contests originalist authority from outside the frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, 0.81).
domain_priors:suppression_score(all_men_created_equal__originalist_reading, 0.78).
domain_priors:theater_ratio(all_men_created_equal__originalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__originalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__originalist_reading, "Equality as Bounded by Founders' Intent (Originalist Reading)").
narrative_ontology:topic_domain(all_men_created_equal__originalist_reading, "constitutional/political").

domain_priors:requires_active_enforcement(all_men_created_equal__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__originalist_reading, 'c93b3fa0-81da-4b9d-bc0f-7a27199edb54').
narrative_ontology:cs_kernel_codification('c93b3fa0-81da-4b9d-bc0f-7a27199edb54', fixed_text).
narrative_ontology:cs_authority_grounding('c93b3fa0-81da-4b9d-bc0f-7a27199edb54', lineage).
narrative_ontology:cs_interpretation_layer_present('c93b3fa0-81da-4b9d-bc0f-7a27199edb54').
narrative_ontology:cs_reading_relation('c93b3fa0-81da-4b9d-bc0f-7a27199edb54', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c93b3fa0-81da-4b9d-bc0f-7a27199edb54', all_men_created_equal__textualist_paradox_reading, influences).
narrative_ontology:cs_axiom('c93b3fa0-81da-4b9d-bc0f-7a27199edb54', foundational, founders_intent_dispositive).
narrative_ontology:cs_axiom_status(founders_intent_dispositive, holdable).
narrative_ontology:cs_axiom_grounding('c93b3fa0-81da-4b9d-bc0f-7a27199edb54', founders_intent_dispositive, conventional).
narrative_ontology:cs_axiom('c93b3fa0-81da-4b9d-bc0f-7a27199edb54', foundational, historical_fidelity_equals_constitutional_meaning).
narrative_ontology:cs_axiom_status(historical_fidelity_equals_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('c93b3fa0-81da-4b9d-bc0f-7a27199edb54', historical_fidelity_equals_constitutional_meaning, deontological).
narrative_ontology:cs_reference_frame('c93b3fa0-81da-4b9d-bc0f-7a27199edb54', founding_era_property_qualified_male_equality).
narrative_ontology:cs_drift_state('c93b3fa0-81da-4b9d-bc0f-7a27199edb54', contemporary_expanded_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c93b3fa0-81da-4b9d-bc0f-7a27199edb54', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__originalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, founding_elite_descendants).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, propertied_classes).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, enslaved_africans).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, indigenous_peoples).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, women).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, non_property_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Descendants of the framers and propertied class of the 1780s. They benefit from an equality doctrine that is authoritatively bounded by the founders' demonstrable intent: that the principle applied only to free white propertied males like themselves. They set the interpretive frame by controlling which historical sources are 'canonical' and which constitutional amendments are treated as clarifications versus departures from original meaning. Their equality is secured; expansion of the principle to newly included groups must surmount the originalist gate.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, founding_elite_descendants, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__originalist_reading, founding_elite_descendants, agenda_setter).

% Economic and political elites who benefit from an equality doctrine that does not disturb property distributions, labor hierarchies, or inheritance structures established during the founding era. Originalism protects their interests by treating expansions of equality (e.g., labor rights, land redistribution, wealth taxation) as departures from the founders' design, requiring supermajority political consensus to overcome.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, propertied_classes, beneficiary,
    powerful, generational, mobile, national).

% Explicitly excluded from the founders' conception of 'men' possessing unalienable rights. The originalist reading anchors that exclusion in historical fact: the founders owned slaves and wrote equality doctrine anyway, proving the principle never intended to cover them. Their enslavement is thus compatible with the constraint as originally understood. No exit; the doctrine that should protect them is weaponized to justify their bondage.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, enslaved_africans, payer,
    powerless, immediate, trapped, national).

% Not citizens of the founding polity; treated as foreign sovereigns in a state of nature outside the social compact. The originalist reading maintains this exclusion: if the founders did not intend to include indigenous peoples in 'all men,' then territorial expansion, removal policies, and denial of citizenship are consistent with the equality principle as authoritatively understood.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, indigenous_peoples, payer,
    powerless, immediate, trapped, national).

% Explicitly not parties to the founding social contract; covered by coverture doctrine under which married women had no independent legal standing. The originalist reading anchors the exclusion of women in historical fact: the founders' intent did not include women in the equality principle, as demonstrated by their own laws and the Nineteenth Amendment (treated as a later, non-originalist revision).
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, women, payer,
    moderate, biographical, constrained, national).

% Free but propertyless persons—agricultural laborers, urban workers, servants. The founders' conception of 'men' possessing inalienable rights implicitly presupposed independence secured by property. The originalist reading treats inequality of property as outside the scope of the equality principle, which protected only equal right to acquire property under conditions the founders established, not equal distribution of property itself.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, non_property_holders, payer,
    powerless, biographical, constrained, national).

% Judges, scholars, and constitutional theorists committed to originalist methodology. They control the frame by determining which historical sources are authoritative, which founder intent counts, and which subsequent amendments represent authentic constitutional change versus extra-constitutional political victories. They adjudicate the constraint's scope and enforce it against expansionist readings.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, originalist_interpretive_community, agenda_setter,
    institutional, generational, analytical, national).

% Social movements (abolitionism, feminism, civil rights, labor organizing) that read the equality principle as universal and therefore as requiring iterative expansion beyond the founders' intent. They are excluded from authoritatively interpreting the constraint; their readings are treated as extra-constitutional political claims, not originalist doctrine. They can win political change, but the originalist gate makes such changes appear to be departures from rather than developments of constitutional meaning.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, universalist_political_movements, excluded,
    organized, generational, constrained, national).

% Scholars and jurists who argue that the universal language of 'all men are created equal' cannot be reconciled with the founders' restrictive application; the gap is not a feature of historical intent but a performative contradiction built into the founding text itself. Their framing threatens the originalist gate by suggesting the contradiction itself is the constitutional fact, not the founders' intent.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, textualist_critics, excluded,
    moderate, biographical, constrained, national).

% Jurists and movements advocating living constitutionalism or progressive interpretation. They observe the originalist reading from outside the interpretive framework; they contest its authority and advocate for alternative methodologies that treat the equality principle as evolving. They have limited institutional power in periods of originalist judicial dominance but can alter the enforcement landscape when they gain bench majorities.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, progressive_judicial_bloc, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__originalist_reading, founding_elite_descendants).
narrative_ontology:fixing_cost_class(all_men_created_equal__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, interpretively bounded doctrine of equal rights: by anchoring equality's scope to founders' demonstrable intent, the originalist reading provides a fixed reference point that resists unlimited expansion and allows political stability around what 'equality' means in constitutional law.
% TRANSFER_FUNCTION: Transfers the power to define which groups merit equal protection from marginalized groups and contemporary majorities to the founding elite's documented intent: it moves political authority backward in time, making the founders' choices dispositive over present circumstances. It transfers material benefit (preservation of property distributions, hierarchies, political power) from excluded groups to beneficiary elites who control the originalist interpretive apparatus.
% ABSENT_VOICES: The enslaved, indigenous peoples, women, and propertyless persons—precisely those the constraint excludes—are not represented in founding-era documents except as objects of property law. Textualist critics and universalist interpreters are not parties to the originalist frame; they would argue the constraint performs a cover function, disguising a restrictive political choice in the language of historical fidelity.
% DISAPPEARANCE_RATIONALE: If the originalist reading and its interpretive authority disappeared, the equality principle would enter open political interpretation: universalist movements would have immediate standing to argue for expansion; the constraint that currently requires supermajority consensus to override the founders' documented exclusions would no longer hold. Civil rights, women's suffrage, labor protections, and anti-discrimination law would no longer need to be justified as departures from originalism but could be read as developments of the principle itself.
% FOUNDING_PROBLEM: Providing a stable, authoritative interpretation of the equality clause that prevents unlimited expansion of rights and maintains constitutional predictability by tethering the meaning to the founders' demonstrable historical intent rather than evolving political demands.
% FOUNDING_PROBLEM_CORROBORATION: Originalist theorists attest the founding problem is live and the constraint essential to constitutional stability. Universalist movements, textualist critics, and historical scholars attest the constraint obscures a performing contradiction: the document says 'all men' but was applied restrictively by design, and treating the restrictive application as the authoritative meaning erases that contradiction rather than resolving it. Testimony from outside the benefiting cohort (abolitionist historical records, feminist jurisprudence, critical race theory scholarship) corroborates that the originalist reading has functioned to protect exclusions rather than expand rights.
narrative_ontology:disappearance_verdict(all_men_created_equal__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(all_men_created_equal__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__originalist_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81) because the constraint protects existing inequalities by treating them as compatible with the founding equality principle—the founders explicitly excluded the groups now claiming equal treatment, and that exclusion becomes binding precedent. Suppression is also high (0.78) because the originalist frame actively excludes competing interpretations; the constraint survives through institutional enforcement (originalist judicial appointments, control of law school curricula, scholarly authority) and through the performative exclusion of universalist and textualist readings from the 'legitimate' interpretive space. Theater is moderate and rising (0.25→0.42 over the interval): early periods feature straightforward historical argument; as political pressure for expansion (civil rights, feminism) accumulates, more theatrical justification is needed—elaborate historical scholarship defending minute details of founder intent, aggressive assertion that any expansion would be 'activist,' and strategic narrowing of which founders' views count as dispositive. The measurements show extractiveness and suppression rising asymptotically; the theater ratio rises steadily, indicating increasing performative work to maintain the constraint as political pressure grows. All three metrics share one time grid (seven points spanning 240 units) as required.
 *
 * PERSPECTIVAL GAP:
 *   The originalist agenda-setter and the payer seats should compute to different types. From the agenda-setter seat (originalist judges, scholars), the constraint computes as rope: genuine coordination problem (stable interpretation) with aligned beneficiaries and minimal suppression (the interpretation just reflects historical fact). From the victim seats (those originally excluded), the constraint computes as snare: the coordination story is cover; persistence depends entirely on institutional power to exclude competing readings; the 'historical fact' is a curated selection (which founders' intent counts, which historical sources are canonical, which later amendments represent authentic change versus departures). The same institutional power that maintains the originalist frame actively suppresses textualist readings and universalist interpretations—that suppression is structural to the constraint. Seat divergence is not a bug; it is the mechanism: the constraint functions precisely by presenting one seat's view (historical fidelity) as neutral, objective interpretation while excluding the alternative readings that would appear equally valid from other seats.
 *
 * DIRECTIONALITY LOGIC:
 *   From the beneficiary seats (founding elite descendants, propertied classes), the constraint is read as genuine coordination—necessary for stable, predictable constitutional meaning. Their d is near 0.0 (beneficiaries; the constraint subsidizes their position). From the payer seats (enslaved, excluded groups, propertyless), the constraint is read as pure extraction—it weaponizes the equality principle against them by anchoring it to the founders' documented exclusions. Their d is near 1.0 (targets; the constraint extracts from them). The originalist interpretive community sits at d ≈ 0.2 (they benefit from institutional authority but frame themselves as neutral arbiters of historical fact; this is a partial-beneficiary position). Progressive and textualist critics sit near d ≈ 0.5 (they pay the cost of being excluded from the interpretive frame; they benefit intellectually from the contradiction it creates, which fuels their arguments; net position is symmetric). The engine computes these from the structural data; divergence between seats is expected and diagnostically meaningful—it is the whole point of the originalist constraint: to produce different effective classifications at different seats.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not resolved; the founding problem remains contested. The constraint's founding justification (providing stable interpretation) is live—originalists genuinely believe stable meaning requires historical fidelity. But the constraint's performance has drifted: it now functions primarily to protect existing power distributions rather than to stabilize interpretation (universalists can point to centuries of stable interpretation under progressive methodologies; the uniqueness of originalism's interpretive stability is claimed, not demonstrated). The theater ratio rising over time (0.25→0.42) signals mandatrophy onset: the constraint requires increasing rhetorical work to justify as interpretive stability accumulates evidence of political motivation. Classification: tangled_rope remains accurate because the constraint DOES perform genuine coordination (stable interpretation) AND it DOES extract asymmetrically (protects beneficiary elites while victimizing excluded groups). These are not alternatives; they are simultaneous. The constraint is not a fallen rope (piton) because the coordination function is genuinely served; it is not a snare because genuine coordination IS happening, not merely a cover story. It is tangled—authentically both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_discernment,
    'Whose intent counts as ''founders'' intent,'' and how are we certain what they intended regarding future expansions of equality?',
    'Comparative historical analysis of founding-era sources (Federalist Papers, debates, letters, laws) versus archaeological reconstruction; test whether different canonical selections yield coherent intent or contradictory intents.',
    'If founders'' intent is indeterminate or contradictory, the originalist constraint loses its empirical grounding—it becomes revealed as selection of convenient historical sources, not objective historical fidelity. The constraint would reclassify from tangled_rope (justified by historical fact) toward snare (justified by institutional power to control interpretation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_discernment, empirical, 'Whether ''founders intent'' can be established as determinate and coherent.').

omega_variable(
    universal_language_paradox,
    'Can the universal language ''all men are created equal'' be authoritatively reconciled with the founders'' restrictive application, or does the gap represent a performative contradiction built into the founding text itself?',
    'Foundational scholarship on performative contradiction in constitutional interpretation; comparison with other universal-language legal texts applied restrictively (international human rights conventions with exclusions).',
    'If the gap is performative contradiction rather than resolvable ambiguity, originalism''s authority derives not from historical fidelity but from institutional power to suppress the textualist reading. The constraint''s classification would shift: from tangled_rope (coordination + asymmetric extraction) toward snare (extraction with performative coordination cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_language_paradox, conceptual, 'Whether the universal language and restrictive application constitute performative contradiction or reconcilable ambiguity.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the suppression of universalist and textualist readings maintained structurally (institutional gatekeeping, resource control, credentialing) or internalized (the excluded readings genuinely appear less coherent or scholarly within the originalist frame)?',
    'Post-exclusion persistence test: if universalist scholars trained in originalist methodology but lacking career stakes in the originalist consensus independently converge on universalist readings, suppression is structural; if they converge on originalism, suppression is internalized.',
    'If suppression is internalized, the constraint is more resilient—removal of institutional barriers would not automatically restore the excluded readings. If structural, removal of originalist institutional dominance would create rapid reversal. Internalized suppression suggests the constraint''s effective extraction is higher than the suppression metric alone indicates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether suppression of alternative readings is structural or internalized.').

omega_variable(
    amendment_reading_complication,
    'Does originalism treat subsequent constitutional amendments (13th, 14th, 15th, 19th, 26th) as clarifications of the original principle or as departures from it?',
    'Originalist scholarship: does it treat the 14th Amendment''s equal protection clause as developing the original principle or as introducing new meaning the founders could not have intended?',
    'If amendments are treated as developments, originalism''s bounding function weakens—the principle itself is acknowledged to evolve. If amendments are departures, originalism creates a class of ''post-originalist'' rights not grounded in the founding, making the constraint more transparently a gate controlling which groups'' claims are treated as constitutional versus political.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_reading_complication, empirical, 'Whether constitutional amendments extend or depart from originalism''s bounded principle.').

omega_variable(
    reading_incommensurability,
    'Is the originalist reading logically incommensurable with the universalist reading (neither can be true in the same framework), or do they occupy different epistemic bases (one privileging historical intent, the other privileging universal principle) and could coexist?',
    'Foundational logic: test whether ''equality bounded by founders'' intent'' and ''equality as universal principle requiring expansion'' are logical contradictories (foreclosed relationship) or merely contrary opinions (coexist relationship). The test: can a single legal framework hold both ''the founders intended this scope'' (factual claim) and ''the principle demands broader scope than founders intended'' (normative claim) without contradiction?',
    'If incommensurable (foreclosed), the originalist reading wins or loses control of the equality principle entirely—one reading determines constitutional meaning. If coexistent (different bases), originalism and universalism compete for judicial and political authority but neither logically rules out the other. This distinction determines whether the engine should classify the reading_relation as ''forecloses'' or ''coexists_with''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_incommensurability, conceptual, 'Whether originalist and universalist readings are logically incommensurable or coexistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__originalist_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t0, all_men_created_equal__originalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(all__tr_t40, all_men_created_equal__originalist_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(all__tr_t80, all_men_created_equal__originalist_reading, theater_ratio, 80, 0.32).
narrative_ontology:measurement(all__tr_t120, all_men_created_equal__originalist_reading, theater_ratio, 120, 0.38).
narrative_ontology:measurement(all__tr_t160, all_men_created_equal__originalist_reading, theater_ratio, 160, 0.4).
narrative_ontology:measurement(all__tr_t200, all_men_created_equal__originalist_reading, theater_ratio, 200, 0.42).
narrative_ontology:measurement(all__tr_t240, all_men_created_equal__originalist_reading, theater_ratio, 240, 0.42).

% Extraction over time
narrative_ontology:measurement(all__be_t0, all_men_created_equal__originalist_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(all__be_t40, all_men_created_equal__originalist_reading, base_extractiveness, 40, 0.71).
narrative_ontology:measurement(all__be_t80, all_men_created_equal__originalist_reading, base_extractiveness, 80, 0.75).
narrative_ontology:measurement(all__be_t120, all_men_created_equal__originalist_reading, base_extractiveness, 120, 0.79).
narrative_ontology:measurement(all__be_t160, all_men_created_equal__originalist_reading, base_extractiveness, 160, 0.8).
narrative_ontology:measurement(all__be_t200, all_men_created_equal__originalist_reading, base_extractiveness, 200, 0.81).
narrative_ontology:measurement(all__be_t240, all_men_created_equal__originalist_reading, base_extractiveness, 240, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t0, all_men_created_equal__originalist_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(all__su_t40, all_men_created_equal__originalist_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(all__su_t80, all_men_created_equal__originalist_reading, suppression_requirement, 80, 0.7).
narrative_ontology:measurement(all__su_t120, all_men_created_equal__originalist_reading, suppression_requirement, 120, 0.75).
narrative_ontology:measurement(all__su_t160, all_men_created_equal__originalist_reading, suppression_requirement, 160, 0.77).
narrative_ontology:measurement(all__su_t200, all_men_created_equal__originalist_reading, suppression_requirement, 200, 0.78).
narrative_ontology:measurement(all__su_t240, all_men_created_equal__originalist_reading, suppression_requirement, 240, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(all_men_created_equal__originalist_reading, 0.18).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, all_men_created_equal__universalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, all_men_created_equal__textualist_paradox_reading).

% DUAL FORMULATION NOTE:
% The 'all men created equal' kernel decomposes into three structurally distinct constraint stories: the originalist_reading (this constraint, high extractiveness, bounded scope), the universalist_reading (universal principle, iterative expansion, low extractiveness for the principle itself but high for those defending the originalist gate), and the textualist_paradox_reading (the contradiction itself is the constitutional fact, creating a different set of beneficiaries/victims—those who profit from highlighting the performative gap). Each reading instantiates different ε values, different victim/beneficiary structures, and different classifications. They are linked via network.affects_constraints because each reading's institutional dominance affects the others' accessibility and credibility. The originalist reading influences the universalist reading by raising the political cost of expansion; the textualist reading influences both by threatening to undermine the authority structure that either could claim. They are NOT variations on one constraint; they are siblings competing for authority over the same kernel text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
