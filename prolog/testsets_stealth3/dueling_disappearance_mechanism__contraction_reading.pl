% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__contraction_reading, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: dueling_disappearance_mechanism__contraction_reading
 *   human_readable: Contraction Reading: Dignity-Culture Displacement of the Honor Substrate
 *   domain: historical sociology / cultural anthropology / legal history
 *
 * SUMMARY:
 *   Between roughly 1750 and 1910 the normative code governing male status
 *   and dispute among European and Atlantic elites inverted: honor culture,
 *   in which a gentleman's worth was publicly displayed and an insult obliged
 *   armed satisfaction, gave way to dignity culture, in which worth is
 *   intrinsic and recourse to private violence marks barbarism. On this
 *   reading the operative mechanism of dueling's disappearance was neither
 *   statute nor substitute institution but axiomatic displacement at the
 *   substrate level: the premises beneath honor reasoning — that standing is
 *   conferred by witnesses, that cowardice is the unforgivable exposure, that
 *   satisfaction restores equality between challengers — were progressively
 *   dismantled, until issuing or accepting a challenge was not prohibited but
 *   unthinkable. The epsilon referent is the standing dignity-culture
 *   arrangement as it governs dispute and status practice, assessed by this
 *   reading's own lights: a substantially emancipatory order whose costs
 *   concentrate on the residual honor class whose normative world it rendered
 *   illegible. The interval maps T=0 to 1750 and T=100 to 1910. KEY AGENTS
 *   (by structural relationship): - honor_culture_practitioners: Primary
 *   target (moderate/identity_locked) — late bearers of the honor code whose
 *   status economy became illegible - young_gentlemen_of_honor_class: Primary
 *   beneficiary (moderate/mobile) — inherit standing without obligatory
 *   violence - bourgeois_dignity_classes: Secondary beneficiary
 *   (organized/mobile) — acquire the vacant authority to define legitimate
 *   standing - kin_of_potential_duelists: Incidental beneficiary
 *   (moderate/constrained) — spared the code's mortal arithmetic -
 *   dueling_code_authors: Excluded voice (moderate/trapped) — the tradition's
 *   theorists, left without discursive standing - historical_sociologists:
 *   Analytical observer (analytical/analytical) — see the full axiomatic
 *   transition from outside all frames
 *
 * KEY AGENTS:
 *   - honor_culture_practitioners — primary target (moderate power, identity_locked exit, continental scope): bear the illegibility cost of the frame change
 *   - young_gentlemen_of_honor_class — primary beneficiary (moderate, mobile): the post-shift cohort relieved of dueling obligation
 *   - bourgeois_dignity_classes — secondary beneficiary (organized, mobile): acquire the status-authority the displacement vacates
 *   - kin_of_potential_duelists — incidental beneficiary (moderate, constrained): households spared the old mortality
 *   - dueling_code_authors — excluded voice (moderate, trapped): the honor tradition's silenced theorists
 *   - historical_sociologists — analytical observer (analytical, global): full-structure seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.3).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.08).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.06).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.06).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Contraction Reading: Dignity-Culture Displacement of the Honor Substrate").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "historical sociology / cultural anthropology / legal history").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, '828e8850-e33d-4b5e-acb9-20d369b8e0b0').
narrative_ontology:cs_kernel_codification('828e8850-e33d-4b5e-acb9-20d369b8e0b0', distributed).
narrative_ontology:cs_authority_grounding('828e8850-e33d-4b5e-acb9-20d369b8e0b0', diffuse_epistemic).
narrative_ontology:cs_reading_relation('828e8850-e33d-4b5e-acb9-20d369b8e0b0', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('828e8850-e33d-4b5e-acb9-20d369b8e0b0', dueling_disappearance_mechanism__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('828e8850-e33d-4b5e-acb9-20d369b8e0b0', foundational, dignity_substrate_irreversibility).
narrative_ontology:cs_axiom_status(dignity_substrate_irreversibility, holdable).
narrative_ontology:cs_axiom_grounding('828e8850-e33d-4b5e-acb9-20d369b8e0b0', dignity_substrate_irreversibility, empirically_contingent).
narrative_ontology:cs_axiom('828e8850-e33d-4b5e-acb9-20d369b8e0b0', foundational, cultural_axioms_prior_to_institutions).
narrative_ontology:cs_axiom_status(cultural_axioms_prior_to_institutions, holdable).
narrative_ontology:cs_axiom_grounding('828e8850-e33d-4b5e-acb9-20d369b8e0b0', cultural_axioms_prior_to_institutions, empirically_contingent).
narrative_ontology:cs_reference_frame('828e8850-e33d-4b5e-acb9-20d369b8e0b0', honor_axiom_baseline).
narrative_ontology:cs_drift_state('828e8850-e33d-4b5e-acb9-20d369b8e0b0', contemporary_honor_persistence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('828e8850-e33d-4b5e-acb9-20d369b8e0b0', '2026-08-10T14:22:31Z').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, young_gentlemen_of_honor_class).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, bourgeois_dignity_classes).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, kin_of_potential_duelists).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__contraction_reading, intrinsic_worth_principle).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__contraction_reading, moral_progress_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gentry and officer-class men of the late honor era whose standing rested on publicly displayed courage and the recognized right to demand armed satisfaction for insult. As the dignity frame spread, their code lost legibility: refusing a challenge came to read not as cowardice but as sanity, and invoking the code came to read as barbarism. Their careers, marriages, and institutional places had been built inside the honor economy; adopting the successor frame meant ceasing to be who they were. Many withdrew into shrinking enclaves — particular regiments, corps, and regions — where the old code retained meaning a generation longer.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    moderate, biographical, identity_locked, continental).

% Sons of the same gentry born after the shift. They inherit family standing, education, and profession without any obligation to answer insults with pistols. Where their fathers calculated seconds, witnesses, and odds of survival, they calculate nothing of the kind; the entire apparatus of challenge and refusal is absent from their repertoire. Leaving the old economy costs them nothing because they were never inside it.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, young_gentlemen_of_honor_class, beneficiary,
    moderate, generational, mobile, continental).

% Commercial, professional, and administrative families rising through the eighteenth and nineteenth centuries. Their worth-language — industry, probity, interior character — becomes the only admissible currency of status as the honor code demonetizes. They staff the schools, presses, pulpits, and ministries through which the dignity frame circulates, and they occupy the position of arbiter over which ways of speaking about oneself and one's enemies remain intelligible. The displacement hands them the vacant authority to define legitimate standing.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, bourgeois_dignity_classes, beneficiary,
    organized, generational, mobile, continental).

% Parents, wives, and children of men who, under the old code, stood a real chance of being challenged or of being honor-bound to issue a challenge. Each generation the shift reaches converts mortal risk into ordinary family continuity; households that had implicitly budgeted for widowhood and fatherless estates stop budgeting for them.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, kin_of_potential_duelists, beneficiary,
    moderate, biographical, constrained, national).

% The honor tradition's own intellectuals: compilers of dueling codes, apologists, and veteran duelists turned theorists. They possessed an elaborate casuistry for why armed satisfaction was the rational settlement of insult among equals. By the time the dignity frame consolidated, those arguments could no longer be posed in respectable company — objection was not answered but made unspeakable. Their treatises survive as curiosities rather than as participants in debate.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dueling_code_authors, excluded,
    moderate, biographical, trapped, continental).

% Analysts of the transition working from outside any of the contending frames, on court archives, correspondence, printed codes, and homicide series. They can observe that the honor economy's premises and the dignity economy's premises cannot both govern the same insult, and can trace which premises surviving practice actually assumed at each stage of the interval.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__contraction_reading, bourgeois_dignity_classes).
narrative_ontology:fixing_cost_class(dueling_disappearance_mechanism__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, violence-free standard for establishing personal worth and resolving slights among strangers at commercial-bureaucratic scale. Honor logic solved grievance only inside small, face-to-face status communities where every insult was witnessed and every challenge enforceable; beyond that scale it generates escalating lethal feuding. The dignity frame supplies a common standard of worth that lets unrelated strangers, and superiors and subordinates, interact without continuous honor-auditing.
% TRANSFER_FUNCTION: Moves status-legitimacy and grievance-authority from honor-coded practitioners to dignity-coded actors: public courage-display loses standing as currency while interior-virtue and professional-conduct claims become the sole admissible worth language. The honor class's accumulated normative capital is demonetized without compensation; the capacity to declare which self-presentations count as legitimate accrues to the dignity-coded classes.
% ABSENT_VOICES: The honor-code authors and theorists would object if they could: their normative universe was dismantled without a hearing, and by the time the dignity frame consolidated, arguing for satisfaction-by-arms was no longer answerable discourse but unintelligible speech. They are absent not because they conceded but because the frame change removed the discursive space in which their objection could be posed. Their seat is recorded as commentary-grade absence, not as a correction input.
% DISAPPEARANCE_RATIONALE: If dignity-culture axioms vanished overnight and honor logic returned to thinkability, dispute practice among status-conscious men would reorganize around public challenge and armed satisfaction; parliaments, professions, presses, and military hierarchies — all built assuming non-violent grievance processing — would face immediate legitimacy strain; and the entire etiquette of insult management would invert, since endurance of contempt would again read as dishonor rather than maturity.
% FOUNDING_PROBLEM: How can a large, anonymous, commercially integrated society establish personal standing and resolve insults without privatized lethal violence — a problem honor governance provably could not solve beyond small face-to-face elites, where every slight demanded satisfaction and every satisfaction bred retaliation?
% FOUNDING_PROBLEM_CORROBORATION: Corroboration comes from outside the beneficiary set. The honor tradition's own defenders — code compilers such as the Clonmel convention and Lyde Wilson's code, and their apologists — attested from the payer seat that grievance among gentlemen was a real problem demanding settlement, while rejecting the dignity frame's answer. Modern quantitative historians of violence independently attest that pre-modern elite dispute practice was lethally unstable and that its pacification tracks the cultural transition, not merely statute dates. No attesting source inside the dignity-culture beneficiary set is relied upon for the genealogy.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dueling_disappearance_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The contraction reading holds the displacement to be structurally mountainous: it operates at the level of the conceptual substrate, persists with no enforcement apparatus attached, and is claimed irreversible — hence claimed_type mountain with emerges_naturally true, asserted from this reading's own lights. The metrics are authored as descriptive facts about the standing arrangement's operation. Suppression is authored low (0.08) because unthinkability requires no coercive maintenance — the reading's central evidence is precisely the absence of any machinery keeping the old code down; per the standing rule, suppression is treated as a raw structural property and is not scaled by power or scope anywhere in this analysis. Accessibility collapse is high (0.88): once the dignity axioms are internalized the honor alternative does not merely lose availability, it loses conceivability. Resistance is low-to-moderate (0.22): holdout pockets — the antebellum American South, continental officer corps — sustained the old logic for decades before dissolving, and transient revivals enter the record as local perturbations the series smooths rather than trend reversals. Theater is near zero (0.06): axioms do not perform maintenance. Extractiveness is authored at 0.30: modest, because the reading assesses the arrangement as substantially emancipatory, but nonzero because the frame change imposed a real, concentrated illegibility cost on practitioners whose careers and selves were built inside the superseded code — a cost that accumulates across the interval as their world contracts (rising base_extractiveness series). The suppression_requirement series falls monotonically: soft persuasive enforcement — clerical condemnation, literary ridicule, editorial sanction — retires as the frame achieves self-sustainment; statutory prohibition lies outside this story's epsilon referent and belongs to the sibling files. All three series share one time grid ({0, 20, 40, 60, 80, 100}). The deliberate divergence between the mountain claim and the declared beneficiary set is the datum: a mountain claim seated on named beneficiaries is exactly the false-summit configuration, and the schema-required omega keeps the natural-law-versus-constructed question open rather than letting the claim certify itself. Receipt surface, authored on its own evidence: gains from the arrangement's operation demonstrably accrue to the bourgeois_dignity_classes seat, which acquires the vacant authority to define legitimate standing — receipt of gain, not merely beneficiary-role. Fixing the arrangement — reconstituting honor thinkability — is prohibitively costly for any actor: the material bases of the honor economy (hereditary status display, witness-dependent standing, elite monopoly on arms) no longer exist to be rebuilt upon.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently because the frame change strikes them asymmetrically. From the honor_culture_practitioner seat the arrangement is the annihilation of a normative universe: every institution that once certified their worth now reads their code as pathology; the same order that looks emancipatory from the young_gentleman seat looks like expropriation without a hearing from theirs. The sharpest divergence runs inside one nominally identical class, split by generation — fathers fused with the code, sons born outside it. The identity-lock binding the payer seat is professional-relational fusion: courage-display was not a strategy the self pursued but the substance of the self, so exit by frame-switching equals self-annihilation, which is why their exit atom is identity_locked rather than merely constrained. If that fusion broke — as it did, gradually, across the cohort's descendants — the seat's computed classification would converge toward the mobile-beneficiary profile of their sons; the lock is cohort-specific, not perpetual. The analytical seat observes all of this symmetrically. The excluded seat experienced a distinctive silence: an objection never refuted, only rendered unspeakable.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derived directionalities, and no overrides are authored. honor_culture_practitioners, the declared victim with identity_locked exit, derives near the full-target end — the amplifying case, since their illegibility cost cannot be exited by frame-switching, so effective extraction is amplified rather than escaped. The three beneficiary groups derive near the subsidy end: kin_of_potential_duelists nearest zero d (pure incidental relief), young_gentlemen_of_honor_class close behind (relief plus inherited standing), bourgeois_dignity_classes carrying a slight upward pull from pure subsidy because their gain arrives packaged with the dignity frame's own disciplines — grievances must now be endured or bureaucratized — without approaching symmetry. The only intra-power-atom ambiguity (two moderate-power beneficiary groups) resolves cleanly from exit differences (mobile versus constrained), so the derivation chain suffices and overrides would add nothing. Scope effects: the arrangement operates at continental scope, where verification of any counter-norm's survival is hardest, which the engine weighs in scaling effective extraction; suppression, again, enters unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   Classification here guards against both symmetrical mislabelings. Treating the displacement as pure emancipation (a rope of cultural progress) erases the concentrated, named cost borne by the honor practitioners whose world became illegible; treating it as pure predation (a snare) ignores the genuine, still-live coordination function and the absence of any administrative capturer running the arrangement. Mandatrophy is not resolved-and-dead: the founding problem — non-violent dispute resolution among strangers at scale — is still live, and the disappearance verdict is world_rearranges, so no zombie flag fires and no sunset machinery applies. The live risk is false-summit: the mountain claim sits atop a declared beneficiary set, and the FSM signature plus the schema-required omega hold the natural-law-versus-constructed question open for the engine rather than letting the reading's own irreversibility thesis certify itself. The receipt surface sharpens this: gains accrue to a named seat while the arrangement claims enforcement-independence — a tension the engine, not the author, adjudicates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_substrate_vs_constructed_shift,
    'Is the dignity-culture displacement a genuine irreversible substrate of modern social organization (natural-law-like), or a contingent, constructed normative shift with identifiable winners and losers?',
    'Comparative anthropology of honor/dignity transitions: test whether honor logics reliably re-emerge wherever dignity-culture monitoring relaxes (herding-culture studies of the American South, post-imperial and post-Soviet honor revivals, diaspora honor-violence persistence). Reliable re-emergence under relaxation indicates construction; permanent absence indicates substrate.',
    'If a genuine substrate, the mountain-type claim is sustained and the arrangement reads as a civilizational limit; if constructed, the declared beneficiary set routes the story through false-summit handling toward hybrid coordination/extraction classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_substrate_vs_constructed_shift, empirical, 'Whether the dignity substrate is natural law or a constructed shift with beneficiaries (schema-required FSM ambiguity omega).').

omega_variable(
    sibling_reading_epsilon_delta,
    'How would classification change if a sibling reading of the dueling_disappearance_mechanism kernel were adopted as the operative constraint instead of this contraction reading?',
    'Re-reference epsilon onto the sibling''s arrangement: the institutional-displacement sibling''s referent is the dispute-resolution market (courts, banking credit, libel law) with victims drawn from those those institutions served poorly; the overdetermined-composite sibling aggregates mechanisms and distributes epsilon across channels. Re-derive beneficiaries and victims from each referent and recompute.',
    'Under the institutional sibling, the victim set changes from honor practitioners to poorly served litigants and debtors, and the type likely shifts from this reading''s mountain claim toward actively administered arrangements; the three files remain linked via network edges and are different constraints with different epsilon, not one constraint viewed from angles.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_epsilon_delta, conceptual, 'Committer routing: this story is the contraction reading of a contested kernel; sibling readings instantiate structurally different constraints.').

omega_variable(
    displacement_irreversibility,
    'Is dueling''s unthinkability actually permanent, or can honor logics re-emerge once the surrounding dignitary frame stops being actively reproduced?',
    'Longitudinal and counterfactual analysis of honor-subculture persistence: culture-of-honor experimental replications in the American South, honor-violence rates in weak-state regions, officer-corps retention of archaic challenge customs into the twentieth century.',
    'Demonstrated reversibility collapses the substrate-irreversibility axiom and demotes the mountain claim toward reversible coordination-type classifications; confirmed permanence hardens the mountain claim and raises confidence in the authored accessibility_collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_irreversibility, empirical, 'Empirical status of the reading''s core irreversibility thesis.').

omega_variable(
    illegibility_cost_magnitude,
    'How large is the actual harm borne by honor practitioners whose framework became illegible — persecution-grade (career destruction, forced withdrawal from institutions, social death) or benign-obsolescence grade?',
    'Prosopographic tracking of late-duelist cohorts: careers, incomes, institutional access, marriage and office outcomes versus matched peers who converted early to the dignity frame.',
    'Persecution-grade harm lifts epsilon materially above the authored 0.30 and strengthens the payer-seat classification; negligible realized harm lowers epsilon toward a near-pure-emancipation profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(illegibility_cost_magnitude, empirical, 'Magnitude of the concentrated cost the frame change imposed on the superseded class.').

omega_variable(
    internalized_unthinkability,
    'Is dueling''s unthinkability an internalized cognitive closure (later generations literally cannot reconstruct why their ancestors fought) or an externally policed discursive exclusion (favorable mention of the practice draws sanction)?',
    'Discourse analysis: test whether pro-dueling argument circulates freely today without sanction, and whether naive reconstructions of the honor frame arise spontaneously in historical imagination.',
    'Internalized closure supports the deep-substrate reading and the low authored suppression; evidence of external policing indicates suppressed-alternative dynamics and raises effective suppression above the authored 0.08.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_unthinkability, empirical, 'Structural versus internalized mechanism behind the disappearance of the honor alternative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t0, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement_basis(duel_tr_t0, observed).
narrative_ontology:measurement(duel_tr_t20, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 20, 0.03).
narrative_ontology:measurement_basis(duel_tr_t20, observed).
narrative_ontology:measurement(duel_tr_t40, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 40, 0.04).
narrative_ontology:measurement_basis(duel_tr_t40, observed).
narrative_ontology:measurement(duel_tr_t60, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement_basis(duel_tr_t60, observed).
narrative_ontology:measurement(duel_tr_t80, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 80, 0.06).
narrative_ontology:measurement_basis(duel_tr_t80, observed).
narrative_ontology:measurement(duel_tr_t100, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 100, 0.06).
narrative_ontology:measurement_basis(duel_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(duel_be_t0, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(duel_be_t0, observed).
narrative_ontology:measurement(duel_be_t20, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement_basis(duel_be_t20, observed).
narrative_ontology:measurement(duel_be_t40, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement_basis(duel_be_t40, observed).
narrative_ontology:measurement(duel_be_t60, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 60, 0.23).
narrative_ontology:measurement_basis(duel_be_t60, observed).
narrative_ontology:measurement(duel_be_t80, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 80, 0.27).
narrative_ontology:measurement_basis(duel_be_t80, observed).
narrative_ontology:measurement(duel_be_t100, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 100, 0.3).
narrative_ontology:measurement_basis(duel_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t0, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(duel_su_t0, observed).
narrative_ontology:measurement(duel_su_t20, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 20, 0.34).
narrative_ontology:measurement_basis(duel_su_t20, observed).
narrative_ontology:measurement(duel_su_t40, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement_basis(duel_su_t40, observed).
narrative_ontology:measurement(duel_su_t60, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 60, 0.17).
narrative_ontology:measurement_basis(duel_su_t60, observed).
narrative_ontology:measurement(duel_su_t80, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 80, 0.11).
narrative_ontology:measurement_basis(duel_su_t80, observed).
narrative_ontology:measurement(duel_su_t100, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 100, 0.08).
narrative_ontology:measurement_basis(duel_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__institutional_displacement_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle. The colloquial label 'why dueling disappeared' conflates three structurally distinct claims with different epsilon referents, different beneficiary/victim structures, and different empirical statuses; each is authored as its own story and linked here. This file instantiates the contraction member (referent: the standing dignity-culture arrangement; epsilon 0.30 assessed by this reading's lights). The institutional-displacement member's referent is the dispute-resolution market, where courts, banking credit, and libel law outcompeted the duel; it carries denser documentary evidence and functions as the family's upstream anchor. This reading treats institutional substitution as downstream expression of the deeper axiomatic shift, so citation pressure flows from the contraction claim back onto the institutional account; the composite member aggregates both plus statutory and wartime-trauma channels. Edges are family-linkage edges, not endorsements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
