% ============================================================================
% CONSTRAINT STORY: citizenship_clause__wong_kim_ark_settlement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_citizenship_clause__wong_kim_ark_settlement_reading, []).

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
 *   constraint_id: citizenship_clause__wong_kim_ark_settlement_reading
 *   human_readable: Wong Kim Ark Settlement Reading of the Citizenship Clause
 *   domain: constitutional_law/citizenship
 *
 * SUMMARY:
 *   Wong Kim Ark (1898) settled a century-old constitutional dispute by
 *   holding that children born on U.S. territory to alien parents are U.S.
 *   citizens under the 14th Amendment's Citizenship Clause, unless excepted
 *   by diplomatic immunity or similar status. The Court adopted a common-law
 *   territorial rule: jus soli (birth on the soil) governs, with narrow
 *   exceptions. This reading treats the settlement as binding and
 *   unchallengeable — Wong is not a document to be reinterpreted but a
 *   stabilized precedent that has structured a century of reliance. The
 *   extractiveness lies in suppressing reopening: revisionist readings that
 *   would emphasize 'subject to the jurisdiction' as requiring allegiance
 *   (not mere territorial presence) are functionally foreclosed not by
 *   logical impossibility but by precedential weight and institutional
 *   embedding. Settled-expectation holders — citizenship adjudicators,
 *   immigrants who acquired citizenship under Wong, government agencies
 *   administering immigration law — benefit from the rule's stability and
 *   clarity. Revisionist advocates and allegiance-reading proponents bear the
 *   cost of suppression: they cannot propose their reading in litigation
 *   without overruling a century-old settlement, a threshold so high that it
 *   functions as effective foreclosure. The constraint is Tangled Rope:
 *   genuine coordination function (clear rule enables planning and
 *   adjudication) overlaid with extraction (suppression of alternative
 *   readings through precedential weight rather than logical refutation).
 *
 * KEY AGENTS:
 *   - Settled-Expectation Holders (institutional/arbitrage): Immigration agencies, citizenship adjudicators, property-holding classes, immigrants who acquired citizenship under Wong — all benefit from the settlement's stability and clarity
 *   - Revisionist Reading Advocates (powerless/trapped): Legal scholars, litigants, constitutional originalists arguing for an allegiance-centered reading — functionally foreclosed by precedent
 *   - Allegiance-Reading Proponents (moderate/constrained): Constitutional reformers seeking to revisit the 'subject to the jurisdiction' language — constrained by institutional weight but retain political voice
 *   - Immigration Authority (moderate/constrained): Implementation actors (USCIS, state courts adjudicating citizenship) — benefit from clarity but suppressed in discretion
 *   - Constitutional Reform Coalition (organized/constrained): Organized advocates for citizenship law reform — experience suppression through precedent, coordination benefit from settled doctrine
 *   - Analytical Observer (analytical/analytical): Civilizational perspective risking naturalization of a contingent institutional settlement as doctrinal mountain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(citizenship_clause__wong_kim_ark_settlement_reading, 0.38).
domain_priors:suppression_score(citizenship_clause__wong_kim_ark_settlement_reading, 0.62).
domain_priors:theater_ratio(citizenship_clause__wong_kim_ark_settlement_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(citizenship_clause__wong_kim_ark_settlement_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(citizenship_clause__wong_kim_ark_settlement_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(citizenship_clause__wong_kim_ark_settlement_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(citizenship_clause__wong_kim_ark_settlement_reading, tangled_rope).
narrative_ontology:human_readable(citizenship_clause__wong_kim_ark_settlement_reading, "Wong Kim Ark Settlement Reading of the Citizenship Clause").
narrative_ontology:topic_domain(citizenship_clause__wong_kim_ark_settlement_reading, "constitutional_law/citizenship").

domain_priors:requires_active_enforcement(citizenship_clause__wong_kim_ark_settlement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(citizenship_clause__wong_kim_ark_settlement_reading, '0721e1df-9d14-4243-8e52-ec2730c8a7f2').
narrative_ontology:cs_kernel_codification('0721e1df-9d14-4243-8e52-ec2730c8a7f2', fixed_text).
narrative_ontology:cs_authority_grounding('0721e1df-9d14-4243-8e52-ec2730c8a7f2', lineage).
narrative_ontology:cs_interpretation_layer_present('0721e1df-9d14-4243-8e52-ec2730c8a7f2').
narrative_ontology:cs_reading_relation('0721e1df-9d14-4243-8e52-ec2730c8a7f2', citizenship_clause__allegiance_qualified_reading, coexists_with).
narrative_ontology:cs_reading_relation('0721e1df-9d14-4243-8e52-ec2730c8a7f2', citizenship_clause__birthright_territorial_reading, influences).
narrative_ontology:cs_axiom('0721e1df-9d14-4243-8e52-ec2730c8a7f2', foundational, precedent_forecloses_reopening).
narrative_ontology:cs_axiom_status(precedent_forecloses_reopening, holdable).
narrative_ontology:cs_axiom_grounding('0721e1df-9d14-4243-8e52-ec2730c8a7f2', precedent_forecloses_reopening, conventional).
narrative_ontology:cs_axiom('0721e1df-9d14-4243-8e52-ec2730c8a7f2', foundational, reliance_trumps_first_principles).
narrative_ontology:cs_axiom_status(reliance_trumps_first_principles, holdable).
narrative_ontology:cs_axiom_grounding('0721e1df-9d14-4243-8e52-ec2730c8a7f2', reliance_trumps_first_principles, instrumental).
narrative_ontology:cs_reference_frame('0721e1df-9d14-4243-8e52-ec2730c8a7f2', wong_territorial_jus_soli).
narrative_ontology:cs_drift_state('0721e1df-9d14-4243-8e52-ec2730c8a7f2', contemporary_immigration_contestation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0721e1df-9d14-4243-8e52-ec2730c8a7f2', '').
narrative_ontology:cs_kernel_id(citizenship_clause__wong_kim_ark_settlement_reading, citizenship_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(citizenship_clause__wong_kim_ark_settlement_reading, settled_expectation_holders).
narrative_ontology:constraint_beneficiary(citizenship_clause__wong_kim_ark_settlement_reading, institutional_stability_dependents).
narrative_ontology:constraint_victim(citizenship_clause__wong_kim_ark_settlement_reading, revisionist_reading_advocates).
narrative_ontology:constraint_victim(citizenship_clause__wong_kim_ark_settlement_reading, jurisdictional_allegiance_proponents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REVISIONIST ADVOCATE / ALLEGIANCE READING (SNARE) — Trapped by precedential weight. A litigant or scholar arguing that 'subject to the jurisdiction' requires allegiance (not mere territorial presence) faces a century of settled reliance blocking reopening. The revisionist position is structurally foreclosed without formal overruling — suppression operates through stare decisis, not legal prohibition, but the functional effect is total. No exit from the precedential constraint without national-level constitutional reinterpretation.
constraint_indexing:constraint_classification(citizenship_clause__wong_kim_ark_settlement_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BIRTHRIGHT TERRITORIAL ADVOCATE / PURE POSITION (SNARE) — This reading advocates for the same territorial rule that Wong settled, but from a position that must defend against allegiance challenges. Trapped in the necessity of defending via precedent rather than first-principles argument. The settlement reading extracts the benefit of doctrinal stability; the pure territorial position must constantly re-defend the same rule against renewal of the underlying contest.
constraint_indexing:constraint_classification(citizenship_clause__wong_kim_ark_settlement_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: IMMIGRATION AUTHORITY / IMPLEMENTATION ACTOR (TANGLED ROPE) — Immigration agencies and citizenship adjudicators benefit from settled doctrine (clear operational rules, immunity from reopening constitutional questions). But they are also constrained by it — their discretion is suppressed by precedent, and they cannot accommodate novel facts that might fit an allegiance reading better. Experiences both coordination (certainty of rule) and extraction (inability to adapt interpretation).
constraint_indexing:constraint_classification(citizenship_clause__wong_kim_ark_settlement_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SETTLED EXPECTATION HOLDER (ROPE) — Institutional beneficiary. Citizenship adjudicators, government agencies, property-holding classes, and immigrants who acquired citizenship under Wong rely entirely on the settlement's stability. The constraint appears as pure coordination: a binding rule that enables planning, reliance, and legal certainty. Zero extraction from this perspective — the settlement is a common good they benefit from.
constraint_indexing:constraint_classification(citizenship_clause__wong_kim_ark_settlement_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL REFORM COALITION (TANGLED ROPE) — Organized advocates (immigration reformers, sovereignty nationalists, constitutional originalists) seeking to revisit the allegiance question. They experience the constraint as a mixed mechanism: it coordinates adjudication (benefit) but suppresses their capacity to propose alternatives (extraction). The suppression is real but not absolute — they retain voice through political avenues, even if litigation is foreclosed by precedent.
constraint_indexing:constraint_classification(citizenship_clause__wong_kim_ark_settlement_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / DOCTRINE AS NATURAL LAW (MOUNTAIN) — The civilizational observer might treat Wong's settlement as an immutable doctrinal mountain: 128 years of reliance, institutional embedding, and stare decisis have transformed a contingent judicial settlement into an effectively irreversible constitutional foundation. This perspective risks naturalizing what is actually a contingent institutional arrangement grounded in suppression (precedential weight) and extraction (revisionist readings barred).
constraint_indexing:constraint_classification(citizenship_clause__wong_kim_ark_settlement_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(citizenship_clause__wong_kim_ark_settlement_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(citizenship_clause__wong_kim_ark_settlement_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(citizenship_clause__wong_kim_ark_settlement_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(citizenship_clause__wong_kim_ark_settlement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, increasing over time (0.15 → 0.38). At the 1898 settlement, extractiveness was low because Wong itself was a hard-fought case arguing against common knowledge of territorial jus soli — the ruling had to justify itself. Over the next 50 years, reliance deepened and the cost of reopening rose, increasing extractiveness to 0.28 by 1948 (mid-century). By 1998, a century of reliance had been built; the cost of questioning Wong had become prohibitive for any litigant without massive institutional backing, pushing extractiveness to 0.38. The plateau at 0.38 reflects that further deepening of reliance encounters diminishing returns — you cannot extract more from a revisionist advocate by making precedent even heavier once it has become nearly insurmountable. Suppression (0.62): High and increasing (0.25 → 0.62). Early suppression was modest because the territorial rule competed with live allegiance arguments in serious legal circles. By mid-century, suppression began to intensify as settled reliance made allegiance arguments seem increasingly quixotic. By 1998, suppression reached 0.62 — reopening requires formal Supreme Court overruling, a threshold almost never met, and courts now treat revisionist readings as historically settled, not as live doctrinal alternatives. Theater ratio (0.55): Moderate. The settlement reading carries real functional content (it does coordinate adjudication and enables reliance), so theater is not high. But approximately 55% of the constraint's operation is performative: courts cite Wong not to justify territorial jus soli from first principles but to invoke precedential weight and institutional stability, gestures that do work in the doctrinal theater but would not survive first-principles scrutiny if the question were reopened. The increasing theater reflects that as reliance deepens, citation to Wong's reasoning becomes less necessary — pure precedential deference suffices.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is enormous. Settled-expectation holders see pure coordination (Rope) — Wong's rule is a transparent, beneficial shared framework. Revisionist advocates see extraction (Snare) — they are trapped by a precedent that forecloses their legal position without refuting its merits. Immigration authorities see mixed coordination and extraction (Tangled Rope) — certainty of rule coordinates their work, but precedential weight suppresses their discretion. The constitutional reform coalition sees the constraint as contestable but institutionally entrenched (Tangled Rope) — they retain political voice but litigation pathways are closed. The analytical observer risks misclassifying the constraint as immutable doctrine (Mountain) when it is actually a contingent institutional arrangement held in place by suppression. The gap reveals that the settlement reading's power lies not in epistemic truth but in institutional weight.
 *
 * DIRECTIONALITY LOGIC:
 *   The directional relationship is determined by the agent's structural position relative to the suppression mechanism. Settled-expectation holders (institutional/arbitrage) benefit from the constraint's coordination function and face zero cost from precedential suppression — they have arbitrage exits (they can ignore revisionist readings and operate under Wong). Revisionist advocates (powerless/trapped) face maximum suppression cost and derive no coordination benefit — they are barred from making their legal argument without overruling a century-old precedent. Immigration authorities (moderate/constrained) experience mixed directionality: they benefit from Wong's clarity (coordination) but are suppressed in their ability to adapt or interpret (extraction). The constitutional reform coalition (organized/constrained) retains voice and political pathways even if litigation is foreclosed. The analytical observer (analytical/analytical) risks misinterpreting institutional suppression as logical necessity. The engine derives d from these structural positions and applies the sigmoid function f(d) to produce experienced extractiveness chi for each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the settlement reading's classification depends on the distinction between logical foreclosure and institutional suppression. If Wong's reasoning logically forecloses the allegiance reading (allegiance and territoriality are incompatible premises), then Wong forecloses the sibling reading and the constraint is pure coordination with no extraction. If Wong's settlement merely suppresses reopening through precedential weight without refuting allegiance on the merits, then the constraint exhibits extraction: revisionist advocates are barred from making legal arguments that survive first-principles analysis. The institutional embedding (128 years of reliance) increases suppression over time, increasing extractiveness proportionally. The constraint is Tangled Rope because it coordinates adjudication (genuine function) while suppressing alternative readings (genuine extraction). The analytical observer's mountain classification is a false summit — what appears as unchangeable natural law is actually a contingent institutional settlement that could be reversed by constitutional amendment or a sufficiently determined Supreme Court overruling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    precedential_foreclosure_mechanism,
    'Does Wong''s settlement foreclose the allegiance reading through inherent logical contradiction, or through institutional/procedural suppression of reopening?',
    'Doctrinal analysis of whether allegiance and territorial readings are logically incompatible or merely politically/institutionally incompatible. Hypothetical: if stare decisis did not exist, could allegiance reading coexist with territorial reading in a single constitutional framework?',
    'If logical foreclosure: Wong forecloses allegiance reading (relation: forecloses). If institutional suppression only: readings coexist but the constraint suppresses one (relation: coexists_with). This determines which axiom status Wong holds: foundational vs enforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(precedential_foreclosure_mechanism, conceptual, 'Whether Wong''s settlement forecloses allegiance reading logically or institutionally').

omega_variable(
    reliance_depth_and_reversibility,
    'How deep is institutional reliance on the Wong settlement? Could a future Supreme Court decision recognizing an allegiance exception function without triggering catastrophic system failure?',
    'Empirical mapping of reliance points: citizenship records, immigration law, property holdings, benefit eligibility, electoral participation. Estimate of remedial burden if one cohort''s citizenship status were rendered ambiguous.',
    'If reliance is shallow or grandfathering feasible: suppression is contingent and reversible. If reliance is deep and systemic: suppression is effectively permanent without constitutional amendment. This affects the characterization of the constraint as Tangled Rope (reversible) vs Snare (irreversible for revisionist advocates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliance_depth_and_reversibility, empirical, 'Depth of institutional reliance and feasibility of revisiting the settlement').

omega_variable(
    kernel_reading_contest_live_status,
    'Is the allegiance reading genuinely live in contemporary constitutional discourse, or has it been formally abandoned within the interpretive tradition?',
    'Citation analysis of legal scholarship, judicial dicta, and constitutional law treatises over the last 20 years. Count of serious advocates for allegiance reading vs territorial reading.',
    'If allegiance reading is live: readings truly coexist (coexists_with). If abandoned or marginal: the contest is historical, and Wong''s settlement is the undisputed reading (no real sibling). This affects omega assignment and the characterization of suppression as active vs historical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_live_status, empirical, 'Whether allegiance reading remains live in contemporary constitutional debate').

omega_variable(
    bounded_vs_unbounded_jurisdiction_reading,
    'Is the ''subject to the jurisdiction'' clause actually contested between allegiance vs territorial, or is the real dispute about scope (some children excluded on other grounds — diplomatic immunity, invading armies, Indian tribes)?',
    'Exegetical analysis of Wong text and contemporaneous commentary. Does Wong settle only the general territorial rule or also the boundaries (diplomatic, military, tribal exceptions)?',
    'If scope is the real dispute: the three readings all agree on territorial baseline and differ on exceptions. Then suppression operates at a finer grain — revisionist readings are not foreclosed on territorial principle but rather on the interpretation of exceptions. This refines the victim set: not all revisionist readings are barred, only those that would collapse the exception categories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bounded_vs_unbounded_jurisdiction_reading, conceptual, 'Whether the fundamental dispute is allegiance vs territorial or scope of exceptions within territorial rule').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(citizenship_clause__wong_kim_ark_settlement_reading, 1898, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wong_theater_1898, citizenship_clause__wong_kim_ark_settlement_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(wong_theater_2026, citizenship_clause__wong_kim_ark_settlement_reading, theater_ratio, 128, 0.55).

% Extraction over time
narrative_ontology:measurement(wong_extractiveness_1898, citizenship_clause__wong_kim_ark_settlement_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(wong_extractiveness_1948, citizenship_clause__wong_kim_ark_settlement_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(wong_extractiveness_1998, citizenship_clause__wong_kim_ark_settlement_reading, base_extractiveness, 100, 0.38).
narrative_ontology:measurement(wong_extractiveness_2026, citizenship_clause__wong_kim_ark_settlement_reading, base_extractiveness, 128, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(wong_suppression_1898, citizenship_clause__wong_kim_ark_settlement_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(wong_suppression_1948, citizenship_clause__wong_kim_ark_settlement_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(wong_suppression_1998, citizenship_clause__wong_kim_ark_settlement_reading, suppression_requirement, 100, 0.62).
narrative_ontology:measurement(wong_suppression_2026, citizenship_clause__wong_kim_ark_settlement_reading, suppression_requirement, 128, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(citizenship_clause__wong_kim_ark_settlement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(citizenship_clause__wong_kim_ark_settlement_reading, citizenship_clause__allegiance_qualified_reading).
narrative_ontology:affects_constraint(citizenship_clause__wong_kim_ark_settlement_reading, citizenship_clause__birthright_territorial_reading).

% DUAL FORMULATION NOTE:
% The citizenship_clause kernel has three constraint stories corresponding to three competing readings. Wong settlement reading treats the kernel as settled by precedent; allegiance reading treats the kernel as unsettled and reopenable; birthright territorial reading treats the kernel as supporting territorial jus soli but without precedential settlement. Each reading has its own extractiveness value reflecting its doctrinal status: settled (0.38), marginal/revisionist (0.65+), vs core/uncontested (0.12). All three are linked via network.affects_constraints to show family relationship and doctrinal interdependence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
