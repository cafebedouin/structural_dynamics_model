% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__living_constitutionalist_reading, []).

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
 *   constraint_id: constitutional_text_authority__living_constitutionalist_reading
 *   human_readable: Living-Constitution Interpretive Regime (Living Constitutional Reading of Textual Authority)
 *   domain: constitutional law / legal theory / interpretive jurisprudence
 *
 * SUMMARY:
 *   This story instantiates one reading of the constitutional_text_authority
 *   kernel: the living_constitutionalist_reading. Under this reading the
 *   Constitution's authoritative content is not fixed at ratification —
 *   meaning evolves with social attitudes and values, and contemporary moral
 *   principles and ancient values applied to changing circumstances generate
 *   binding constitutional content through adjudication, Brown v. Board
 *   (1954) being the reading's anchor case of constitutional change without
 *   Article V. The constraint this reading instantiates is an interpretive
 *   regime: the federal courts declare evolved meaning, that content is
 *   supreme law, and legislatures and states must conform to principles they
 *   did not enact. The sibling readings (originalist_reading,
 *   positivist_reading) are separate constraint stories with their own
 *   epsilon, beneficiary structures, and classifications, linked through
 *   network edges; the committer structure is carried in the omega variables
 *   and kernel_context, not folded into this story's classification. The
 *   epsilon referent is the standing arrangement this story is about — the
 *   living-reading interpretive regime itself — and the value is authored
 *   from the reading's own lights: a candid living constitutionalist treats
 *   the regime as the necessary price of a governable supreme law,
 *   acknowledging the counter-majoritarian cost without calling it
 *   usurpation. The claimed type is my independent structural assessment; the
 *   metrics describe the regime's actual operation; divergences between them,
 *   and between either and the engine's per-seat computations, are the data
 *   the corpus exists to collect. KEY AGENTS (by structural relationship): -
 *   federal_judiciary: agenda-setter and principal collector
 *   (institutional/arbitrage) — declares binding content through
 *   evolving-principle adjudication; gains authority and docket from the
 *   method it administers - unenumerated_rights_claimants: direct
 *   beneficiaries (moderate/constrained) — protection only through judicial
 *   recognition; no alternative forum at their timescale -
 *   evolving_rights_movements: organized beneficiaries
 *   (organized/constrained) — court-centered strategy because Article V
 *   forecloses the legislative route - elected_legislatures: primary payers
 *   (institutional/constrained) — statutes invalidated under principles they
 *   did not enact; amendment lever practically inaccessible -
 *   state_governments: secondary payers (institutional/constrained) — state
 *   law displaced by nationalized evolving standards; appear as losing
 *   litigants - originalist_adherents: payers by conviction
 *   (organized/constrained) — their ratified-meaning commitment is overridden
 *   in the operative forum; resistance is their only lever -
 *   popular_constitutionalists: excluded voice (moderate/constrained) — deny
 *   judicial finality itself; no seat in the forum their objection targets -
 *   constitutional_scholars: analytical observers (analytical/analytical) —
 *   map the methods, collect no rents, hold no enforcement power
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__living_constitutionalist_reading, 0.43).
domain_priors:suppression_score(constitutional_text_authority__living_constitutionalist_reading, 0.66).
domain_priors:theater_ratio(constitutional_text_authority__living_constitutionalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, extractiveness, 0.43).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__living_constitutionalist_reading, "Living-Constitution Interpretive Regime (Living Constitutional Reading of Textual Authority)").
narrative_ontology:topic_domain(constitutional_text_authority__living_constitutionalist_reading, "constitutional law / legal theory / interpretive jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__living_constitutionalist_reading, 'c500e848-c284-40f9-a0f9-bbd5d5b84888').
narrative_ontology:cs_kernel_codification('c500e848-c284-40f9-a0f9-bbd5d5b84888', fixed_text).
narrative_ontology:cs_authority_grounding('c500e848-c284-40f9-a0f9-bbd5d5b84888', practice).
narrative_ontology:cs_interpretation_layer_present('c500e848-c284-40f9-a0f9-bbd5d5b84888').
narrative_ontology:cs_reading_relation('c500e848-c284-40f9-a0f9-bbd5d5b84888', constitutional_text_authority__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('c500e848-c284-40f9-a0f9-bbd5d5b84888', constitutional_text_authority__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('c500e848-c284-40f9-a0f9-bbd5d5b84888', foundational, evolving_meaning_is_authoritative).
narrative_ontology:cs_axiom_status(evolving_meaning_is_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('c500e848-c284-40f9-a0f9-bbd5d5b84888', evolving_meaning_is_authoritative, instrumental).
narrative_ontology:cs_axiom('c500e848-c284-40f9-a0f9-bbd5d5b84888', secondary, unenumerated_rights_cognizable).
narrative_ontology:cs_axiom_status(unenumerated_rights_cognizable, holdable).
narrative_ontology:cs_axiom_grounding('c500e848-c284-40f9-a0f9-bbd5d5b84888', unenumerated_rights_cognizable, deontological).
narrative_ontology:cs_reference_frame('c500e848-c284-40f9-a0f9-bbd5d5b84888', evolving_principles_framework).
narrative_ontology:cs_drift_state('c500e848-c284-40f9-a0f9-bbd5d5b84888', contemporary_originalist_ascendancy, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c500e848-c284-40f9-a0f9-bbd5d5b84888', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, evolving_rights_movements).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, elected_legislatures).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, state_governments).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, originalist_adherents).
narrative_ontology:constraint_vindicates(constitutional_text_authority__living_constitutionalist_reading, judicial_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text_authority__living_constitutionalist_reading, common_law_constitutionalism).
narrative_ontology:constraint_vindicates(constitutional_text_authority__living_constitutionalist_reading, evolving_standards_of_decency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Constitution, and its interpretations bind: a principle the courts declare becomes supreme law that legislatures and states must obey. Each generation of judges articulates constitutional content by applying broad principles to present circumstances, and the accumulated line of precedents functions as the operative text. The institution's authority and docket grow with every question the evolving method opens; individual judges can shift between interpretive methods at low personal cost, and the institution as a whole can retrench from or reassert the method across appointment cycles.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__living_constitutionalist_reading, federal_judiciary, beneficiary).

% Litigants whose claimed rights have no explicit textual hook — privacy, bodily autonomy, family formation, dignity interests — obtain protection only through judicial recognition of evolving principle. Their access runs entirely through the courts: no amendment process moves at their timescale, and legislative protection varies by state. When the method retreats, as with overruled autonomy precedents, their protection vanishes with it.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, unenumerated_rights_claimants, beneficiary,
    moderate, biographical, constrained, national).

% National coalitions that have repeatedly achieved through adjudication what the amendment process made unreachable — racial equality, reapportionment, contraception, marriage equality. Their strategy is court-centered because the supermajority requirements of Article V foreclose the legislative route; a retreating Court removes their primary channel and leaves them with no comparably effective alternative.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, evolving_rights_movements, beneficiary,
    organized, generational, constrained, national).

% Congress and the state legislatures enact statutes that the courts may invalidate under principles the legislators did not vote on and cannot revise by ordinary law. Their formal counter-lever, Article V amendment, has assembled only 27 times in 235 years; their practical responses are drafting around holdings, litigating appointments, and jurisdiction-curbing proposals that rarely advance. They bear the arrangement's costs at every session.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, elected_legislatures, payer,
    institutional, biographical, constrained, national).

% State constitutions and statutes are displaced whenever federal courts nationalize an evolving standard — criminal sentencing norms, family law, electoral structures. States appear in the doctrine mostly as losing litigants; their consent to the evolved content is never sought, and their policy diversity is the raw material the national standard overrides. Some states benefit incidentally when a national floor lifts their own preferred policy past local resistance.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, state_governments, payer,
    institutional, biographical, constrained, regional).

% The originalist legal movement and the citizens who share its commitment hold that the ratified meaning is what governs. Under this regime their commitment loses in the operative forum: courts applying evolved principle override the meanings they defend, and their recourse runs through the same courts (appointments, litigation) or an amendment process they cannot assemble. Their professional identity is built around this dispute, which sustains their resistance but offers no exit from the regime's operation.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, originalist_adherents, payer,
    organized, generational, constrained, national).

% Scholars and movements holding that constitutional content should be settled politically by the people rather than finally by courts. Their objection targets judicial finality itself and therefore has no seat in the adjudicative forum it challenges; they influence events from outside — through appointment politics and academic critique — but the regime's operation never adjudicates their claim.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, popular_constitutionalists, excluded,
    moderate, generational, constrained, national).

% The academic interpretive community maps the competing methods, tracks the doctrine's movement over time, and supplies the vocabulary both the Court and its critics use. It holds no enforcement power and collects no direct benefit from the arrangement; its stake is epistemic — which account of constitutional authority the practice vindicates.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(constitutional_text_authority__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the adaptation problem of a fixed written constitution: how an 18th-century text governs changing circumstances without requiring an Article V supermajority for every substantive change, and how constitutional continuity (supremacy, stability, rights protection) can coexist with substantive evolution. The reading routes adaptation through adjudication, preserving a single supreme law while letting its content track contemporary moral understanding.
% TRANSFER_FUNCTION: Moves interpretive authority from the text-as-ratified and from the Article V amendment process to the federal judiciary, and moves binding constitutional content from contemporary legislative majorities to judicially declared evolving principles. Concretely: legislative enactments are invalidated and state policies displaced by doctrines the affected bodies did not enact and cannot amend.
% ABSENT_VOICES: Popular-constitutionalist voices, who deny judicial finality altogether, have no seat in the adjudicative forum their objection targets; the ratifying generations' understanding enters the doctrine only as evidence, never as authority; state sovereigns appear as litigants but not as co-authors of the evolved content that displaces their laws.
% DISAPPEARANCE_RATIONALE: If the living-reading constraint vanished overnight and meaning froze at ratification, the justificatory basis of Brown's school-desegregation holding, one-person-one-vote, incorporation of the Bill of Rights against the states, and the unenumerated-rights line would collapse; the constitutional order would rearrange around either a vastly expanded formal amendment practice or a radically narrowed federal role. Every named seat — the judiciary collecting authority, claimants and movements relying on the channel, legislatures and states bearing displacement, originalist adherents contesting override — is organized around the arrangement's existence.
% FOUNDING_PROBLEM: A written constitution ratified in 1787 cannot anticipate a transformed society; without some adaptive mechanism, the supreme law either becomes progressively obsolete or requires perpetual formal amendment, destabilizing the constitutional order. The living reading was articulated to keep the Constitution governable across generations without sacrificing its supremacy or opening the text to perpetual revision.
% FOUNDING_PROBLEM_CORROBORATION: The problem's liveness is corroborated from outside the beneficiary set: originalist scholars concede the dead-hand problem exists (Scalia's own lectures frame it as the strongest objection his position must answer) while disputing the judicial solution; comparative constitutional scholarship documents formal-amendment scarcity across jurisdictions; the historical record — 27 amendments in 235 years, none supplying routine adaptation — attests that Article V does not deliver the adaptation the founding problem describes. No party, including the regime's opponents, claims the problem is solved by formal amendment alone.
narrative_ontology:disappearance_verdict(constitutional_text_authority__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__living_constitutionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__living_constitutionalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text_authority__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__living_constitutionalist_reading, 0.43, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__living_constitutionalist_reading_tests).
:- end_tests(constitutional_text_authority__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.43, reading-indexed: from the living seat the regime's acknowledged cost is the counter-majoritarian one — binding content imposed on legislatures and states that did not enact it and cannot amend it — which the frame treats as the necessary price of a governable supreme law rather than as usurpation. Suppression (0.66) is structural and unscaled, per the framework's rule that only extractiveness is context-scaled: rival interpretive modes are suppressed as official doctrine through judicial supremacy and stare decisis, while the formal alternative (Article V) persists at prohibitive cost. Theater (0.45) is moderate: the adjudicative practice is mostly functional, but the method's rhetorical apparatus — evolving-standards invocations, legitimacy language, the joint-opinion tradition — carries a performative share that grows when enforcement is contested. Accessibility_collapse (0.45): alternatives do not fully collapse; they persist formally (amendment, appointments, jurisdiction-curbing) at costs that make them practically unreachable — the hybrid signature rather than the closed exit of pure extraction. Resistance (0.65) is high, organized, and currently ascendant: the originalist counter-revolution is the regime's live contest. The measurement series share one grid (1954–2024, eight points) across all three tracked metrics. The trajectory shows accumulation then contestation: extraction climbs with the doctrine's expansion through 2015, then eases as the Dobbs-era retrenchment contracts the regime's footprint; theater rises late as maintenance turns increasingly rhetorical. Suppression_requirement is authored deliberately: this story specifically tracks enforcement-capacity dynamics — the machinery that held the regime in place (supremacy, precedent) now requires more active defense, and the 2024 rise marks that strain. The oscillation in the series is not intermittent reinforcement; it tracks external composition change (appointments), not an internal extraction cycle.
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda-setter seats must compute differently. From the bench, the regime is constitutional method itself: applying principles to new circumstances is what a supreme law is for, and each invalidation is fidelity, not imposition. From the legislature and the statehouse, the same operation is displacement: binding content arrives from an organ they did not elect and cannot revise, and their formal counter-lever has assembled 27 times in 235 years. From the originalist seat the regime is near-maximal taking — the ratified meaning they hold authoritative is overridden without recourse — which is why the same practice this story rates as a moderate, reading-indexed cost would compute as full extraction in the originalist sibling's story. The corpus holds both stories rather than averaging them; the epsilon here is the living seat's honest number. Inter-institutionally, the courts' arbitrage-grade flexibility (they administer the method and can shift methods) contrasts with the legislatures' constrained position (they bear the outputs and cannot revise the inputs). Among same-level payers, Congress and the states hold identical formal levers but differ in exposure: the states supply most of the losing-litigant record, while Congress more often drafts around holdings.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: federal_judiciary (collects interpretive authority; also the agenda-setter — that dual position is the regime's structural signature: the administrator of the arrangement is its principal gainer), unenumerated_rights_claimants and evolving_rights_movements (receive protection and a change channel unavailable elsewhere). Victim declarations: elected_legislatures and state_governments (bear invalidation and displacement), originalist_adherents (bear override of the meaning they hold authoritative). The derivation chain maps these to directionality without overrides: the judiciary sits near the beneficiary end; the constrained payers sit near the target end. I deliberately authored no directionality_overrides: overrides key on the power atom, and this story's seats share atoms across opposed positions — the judiciary, Congress, and the states are all institutional — so an override would misfire across seats; the role declarations already differentiate what the power atoms cannot. Identity-lock dynamics appear on the originalist seat: their professional and ideological identity is constituted against this regime, which explains the intensity of their resistance but does not change their exit class — they cannot leave the constitutional order, only contest it through the regime's own institutions.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. Read as pure coordination — everyone benefits from a governable constitution — the regime's capture disappears from view: the administrator is the principal gainer, the payers have no working exit, and that asymmetry is exactly what the hybrid classification keeps visible. Read as pure extraction — unelected judges rule — the genuine coordination function disappears: the fixed-text adaptation problem is real, corroborated from outside the beneficiary set, and no alternative mechanism currently delivers what adjudication delivers. On obsolescence: the founding problem — a fixed text governing a changing society — is live and permanent, so the regime's mandate is not dead; but the measurements track the obsolescence question where it actually bites. If the living frame's method is abandoned while its precedents persist rhetorically, the theater ratio climbs and the regime drifts toward inertial maintenance — the late-series theater rise is the early signature of that possibility, not yet its realization. The founding_problem_status (live) crossed with disappearance_verdict (world_rearranges) shows no mismatch flag: the arrangement persists because its problem persists, not because its problem is gone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'This story is one reading (living_constitutionalist_reading) of the constitutional_text_authority kernel — what structurally changes if a sibling reading governs instead?',
    'Author the sibling stories (originalist_reading, positivist_reading) as separate constraints and compare beneficiary/victim sets, epsilon, and classification across the family; no data inside this story resolves it.',
    'Under the originalist sibling the structure inverts — fixed-meaning adherents become the beneficiaries and evolving-rights claimants lose their channel; under the positivist sibling the moral-content gate disappears and the regime reduces to enactment-procedure validity with no evolving-rights beneficiary seat at all.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Committer structure: kernel membership and the structural delta each sibling reading would produce.').

omega_variable(
    disagreement_location_meaning_fixity,
    'Where exactly do the readings disagree — on what fixes constitutional content: historical public understanding, contemporary moral principles, or enactment procedure?',
    'Conceptual analysis of the rival authority-granting premises; no empirical dataset resolves what fixes meaning, because the dispute is located in the premise that grants authority, not in any observable downstream of it.',
    'The relation structure between this reading and its siblings follows from the location: the originalist premise directly contradicts this one (no single framework can hold ''meaning is fixed at ratification'' and ''meaning evolves with social attitudes'' of the same text without equivocation on ''meaning''), while the positivist premise operates at a different level — the source of validity rather than the content of meaning — and coexists with this reading, even within single scholars'' commitments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_meaning_fixity, conceptual, 'The structural element the sibling readings differ on is the meaning-fixing authority itself.').

omega_variable(
    epsilon_seat_indexing,
    'The same interpretive practice rates moderate from the living seat and near-maximal from the originalist seat — which seat''s epsilon should govern classification of the regime?',
    'Per the framework''s reading-indexing rule, the corpus holds one story per reading with reading-indexed epsilon over the shared referent; the resolution is comparative — read this story beside the originalist sibling''s story and compare the engine''s per-seat outputs. No averaging.',
    'Averaging would erase the perspectival gap that is the regime''s defining structure; holding both stories preserves it and lets the engine compute the divergence as a measurement rather than dissolving it into a single number.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_seat_indexing, conceptual, 'Epsilon is a property of the reading; the regime''s classification is seat-relative and must not be collapsed across seats.').

omega_variable(
    adaptation_route_separability,
    'Is the adjudicative adaptation route separable from judicial finality — could a popular-constitutionalist or easier-amendment mechanism deliver the same coordination function with less concentrated interpretive authority?',
    'Comparative constitutional data: jurisdictions with easier formal amendment (state constitutions) show lower pressure toward judicial finality; historical popular-constitutionalist episodes (the New Deal court-packing confrontation producing the switch in time) show political adaptation working at scale.',
    'If separable, the concentration of interpretive authority is a contingent institutional choice riding on a real function — raising drift risk toward pure extraction as the function and the capture decouple; if inseparable, part of the measured cost is the price of the coordination itself and the hybrid classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_route_separability, empirical, 'Whether the coordination function requires the concentrated institutional form it currently takes.').

omega_variable(
    brown_anchor_separability,
    'Does Brown''s near-universal legitimacy anchor the whole living frame, or is Brown separable from it — justifiable, as originalists claim, from the Fourteenth Amendment''s original public meaning?',
    'Historiographic and legal scholarship on the reconstruction-era understanding of the Fourteenth Amendment (school segregation''s original treatment) weighed against the living frame''s reliance on Brown as proof that constitutional content changed without Article V.',
    'If Brown is originalist-justifiable, the living frame loses its strongest anchor case and the sibling reading gains ground without contradicting the civil-rights settlement; if not, Brown stands as a case the originalist framework cannot absorb, and this reading''s anchor holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(brown_anchor_separability, empirical, 'Whether the regime''s anchor case is frame-dependent or survives translation into the rival frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__living_constitutionalist_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1954, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 1954, 0.15).
narrative_ontology:measurement_basis(cons_tr_t1954, observed).
narrative_ontology:measurement(cons_tr_t1964, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 1964, 0.18).
narrative_ontology:measurement_basis(cons_tr_t1964, observed).
narrative_ontology:measurement(cons_tr_t1973, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 1973, 0.28).
narrative_ontology:measurement_basis(cons_tr_t1973, observed).
narrative_ontology:measurement(cons_tr_t1984, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 1984, 0.3).
narrative_ontology:measurement_basis(cons_tr_t1984, observed).
narrative_ontology:measurement(cons_tr_t1992, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 1992, 0.38).
narrative_ontology:measurement_basis(cons_tr_t1992, observed).
narrative_ontology:measurement(cons_tr_t2003, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 2003, 0.35).
narrative_ontology:measurement_basis(cons_tr_t2003, observed).
narrative_ontology:measurement(cons_tr_t2015, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 2015, 0.33).
narrative_ontology:measurement_basis(cons_tr_t2015, observed).
narrative_ontology:measurement(cons_tr_t2024, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 2024, 0.45).
narrative_ontology:measurement_basis(cons_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t1954, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 1954, 0.3).
narrative_ontology:measurement_basis(cons_be_t1954, observed).
narrative_ontology:measurement(cons_be_t1964, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 1964, 0.35).
narrative_ontology:measurement_basis(cons_be_t1964, observed).
narrative_ontology:measurement(cons_be_t1973, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 1973, 0.42).
narrative_ontology:measurement_basis(cons_be_t1973, observed).
narrative_ontology:measurement(cons_be_t1984, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 1984, 0.45).
narrative_ontology:measurement_basis(cons_be_t1984, observed).
narrative_ontology:measurement(cons_be_t1992, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 1992, 0.41).
narrative_ontology:measurement_basis(cons_be_t1992, observed).
narrative_ontology:measurement(cons_be_t2003, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 2003, 0.44).
narrative_ontology:measurement_basis(cons_be_t2003, observed).
narrative_ontology:measurement(cons_be_t2015, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 2015, 0.47).
narrative_ontology:measurement_basis(cons_be_t2015, observed).
narrative_ontology:measurement(cons_be_t2024, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 2024, 0.43).
narrative_ontology:measurement_basis(cons_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1954, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 1954, 0.55).
narrative_ontology:measurement_basis(cons_su_t1954, observed).
narrative_ontology:measurement(cons_su_t1964, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 1964, 0.6).
narrative_ontology:measurement_basis(cons_su_t1964, observed).
narrative_ontology:measurement(cons_su_t1973, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 1973, 0.62).
narrative_ontology:measurement_basis(cons_su_t1973, observed).
narrative_ontology:measurement(cons_su_t1984, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 1984, 0.58).
narrative_ontology:measurement_basis(cons_su_t1984, observed).
narrative_ontology:measurement(cons_su_t1992, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 1992, 0.6).
narrative_ontology:measurement_basis(cons_su_t1992, observed).
narrative_ontology:measurement(cons_su_t2003, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 2003, 0.55).
narrative_ontology:measurement_basis(cons_su_t2003, observed).
narrative_ontology:measurement(cons_su_t2015, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement_basis(cons_su_t2015, observed).
narrative_ontology:measurement(cons_su_t2024, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 2024, 0.66).
narrative_ontology:measurement_basis(cons_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority__originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority__positivist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, article_v_amendment_process).

% DUAL FORMULATION NOTE:
% The colloquial label 'how the Constitution means what it means' decomposes into a three-reading constraint family over the single kernel constitutional_text_authority. This story instantiates the living reading; the originalist and positivist siblings are separate files with their own epsilon, beneficiaries, and victims. The family edges run from this reading to both siblings because the living reading's doctrine (Brown, incorporation, evolving standards) is the terrain on which the siblings fight: the originalist sibling claims the same doctrine is derivable from ratified meaning, and the positivist sibling claims its validity flows from institutional acceptance rather than moral content. The link to article_v_amendment_process marks the rival adaptation mechanism the living reading bypasses. Upstream/downstream: the originalist reading (higher empirical anchoring in historical evidence) exerts pressure on this reading's enforcement environment; this reading's accumulated doctrine constrains what the siblings can plausibly claim to preserve.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
