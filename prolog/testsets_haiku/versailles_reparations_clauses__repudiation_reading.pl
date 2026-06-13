% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__repudiation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__repudiation_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: versailles_reparations_clauses__repudiation_reading
 *   human_readable: Versailles Reparations Clauses (Repudiation Reading)
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   The Treaty of Versailles imposed reparations on Germany through Article
 *   231 (War Guilt Clause) after World War I. This story instantiates the
 *   REPUDIATION READING: the treaty was signed under military duress
 *   (occupation, continued blockade, threat of invasion); therefore it is
 *   legally void and Germany bears no binding obligation to pay beyond
 *   symbolic gestures. Under this reading, the reparations clauses are pure
 *   extraction—a snare sustained by occupation enforcement and Allied
 *   military superiority, not by legitimate authority or coordination
 *   function. This reading denies the legitimacy foundation of the entire
 *   settlement. Note: this is one of three readings of the same kernel
 *   (Versailles reparations); the other readings (punitive_liability,
 *   limited_responsibility) occupy different constraint stories with their
 *   own ε values and stakeholder structures. This story presents ONLY the
 *   repudiation reading's structural claim and metrics.
 *
 * KEY AGENTS:
 *   - German state: payer under duress; nominally sovereign but operationally trapped by occupation
 *   - Allied creditor states: institutional beneficiaries; enforce through military occupation and economic sanctions
 *   - German working class: powerless payers; bear material costs through inflation and austerity
 *   - Weimar political leadership: agenda-setter constrained by occupation; theoretical exit is military resistance, practically impossible
 *   - Nationalist political movements: observers who adopt repudiation reading as core claim; gain power by articulating illegitimacy
 *   - International legal community: excluded observers; would defend treaty sanctity but have no enforcement power independent of belligerent states
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, 0.92).
domain_priors:suppression_score(versailles_reparations_clauses__repudiation_reading, 0.88).
domain_priors:theater_ratio(versailles_reparations_clauses__repudiation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__repudiation_reading, snare).
narrative_ontology:human_readable(versailles_reparations_clauses__repudiation_reading, "Versailles Reparations Clauses (Repudiation Reading)").
narrative_ontology:topic_domain(versailles_reparations_clauses__repudiation_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__repudiation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__repudiation_reading, 'e5d12543-359c-4971-9d0d-226035f211cf').
narrative_ontology:cs_kernel_codification('e5d12543-359c-4971-9d0d-226035f211cf', formalized).
narrative_ontology:cs_authority_grounding('e5d12543-359c-4971-9d0d-226035f211cf', extraction).
narrative_ontology:cs_reading_relation('e5d12543-359c-4971-9d0d-226035f211cf', versailles_reparations_clauses__punitive_liability_reading, forecloses).
narrative_ontology:cs_reading_relation('e5d12543-359c-4971-9d0d-226035f211cf', versailles_reparations_clauses__limited_responsibility_reading, influences).
narrative_ontology:cs_axiom('e5d12543-359c-4971-9d0d-226035f211cf', foundational, treaty_void_under_duress).
narrative_ontology:cs_axiom_status(treaty_void_under_duress, holdable).
narrative_ontology:cs_axiom_grounding('e5d12543-359c-4971-9d0d-226035f211cf', treaty_void_under_duress, deontological).
narrative_ontology:cs_axiom('e5d12543-359c-4971-9d0d-226035f211cf', foundational, victor_justice_invalid_without_impartial_authority).
narrative_ontology:cs_axiom_status(victor_justice_invalid_without_impartial_authority, holdable).
narrative_ontology:cs_axiom_grounding('e5d12543-359c-4971-9d0d-226035f211cf', victor_justice_invalid_without_impartial_authority, deontological).
narrative_ontology:cs_reference_frame('e5d12543-359c-4971-9d0d-226035f211cf', international_law_consent_doctrine).
narrative_ontology:cs_drift_state('e5d12543-359c-4971-9d0d-226035f211cf', post_wwii_legal_development, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('e5d12543-359c-4971-9d0d-226035f211cf', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, german_state).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, german_working_class).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, german_creditors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, allied_creditor_states).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, allied_public_populations).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, german_creditors_internal).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__repudiation_reading, duress_doctrine_in_treaties).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__repudiation_reading, national_sovereignty_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Forced to sign the Treaty of Versailles under threat of continued military occupation and blockade; legally obligated to transfer wealth annually to Allied creditors as reparations. Under the repudiation reading, the entire constraint is void because coercion voids consent. The state bears the extraction burden directly through budget allocation, currency instability, and constrained sovereignty.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_state, payer,
    moderate, generational, trapped, global).

% Collective beneficiaries of reparations flows; legitimate victors in the war and architects of the settlement. They enforce payment through occupation forces, sanctions, and threat of re-invasion. They claim moral authority grounded in German aggression and victory rights; the repudiation reading denies this frame by asserting the treaty itself is illegitimate.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, allied_creditor_states, beneficiary,
    institutional, generational, arbitrage, global).

% Bears the material cost of reparations through inflation, unemployment, wage suppression, and austerity policies implemented to service debt. They have no seat at the negotiating table and cannot exit the constraint except through political revolution. The repudiation reading frames reparations as unjust extraction imposed on the population that did not wage the war.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_working_class, payer,
    powerless, biographical, identity_locked, national).

% Benefit from reparations inflows nominally intended for war debt relief and reconstruction. However, the repudiation reading problematizes this claim: if the treaty is illegitimate, so too is the public belief in its justice. Citizens in creditor nations are positioned as beneficiaries by the institutional structure but ideologically captured by the treaty's legitimacy narrative.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, allied_public_populations, beneficiary,
    organized, biographical, constrained, national).

% Would object to the repudiation reading by citing state consent doctrine and the sanctity of treaties, but they have no enforcement power independent of the belligerent parties. Legal scholars and international courts cannot reverse a treaty without consent of the parties or a supervening norm. The repudiation reading treats their objections as theoretical rather than dispositive.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, international_legal_community, excluded,
    analytical, generational, analytical, global).

% German banks, industrialists, and foreign investors holding German debt and securities. They face currency collapse and asset seizure under hyperinflation driven by reparations costs. The repudiation reading offers exit through capital flight and loan restructuring, but politically they are trapped: they cannot openly advocate repudiation without appearing unpatriotic.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_creditors_internal, payer,
    powerful, biographical, mobile, global).

% Nominally sets fiscal and diplomatic policy but operates under overwhelming constraint: they must service reparations or face military reoccupation. The repudiation reading offers them a legitimacy path to default, but only if they can build political coalition and military capacity to resist enforcement. Their exit is theoretically possible but practically constrained by occupation forces.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, weimar_political_leadership, agenda_setter,
    institutional, biographical, constrained, national).

% Adopt the repudiation reading as a core political claim: the treaty is illegitimate, reparations are slavery, Germany is a victim of victors' justice. This reading becomes the rallying narrative for rearmament and treaty rejection. They gain political power by articulating what the repudiation reading names: the constraint is extractive, imposed under duress, and morally void.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, nationalist_political_movements, observer,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__repudiation_reading, allied_creditor_states).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__repudiation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — the repudiation reading explicitly denies any coordination function. The treaty imposed a unidirectional transfer, not a solution to a collective action problem. No coordination problem required its structure; pure power asymmetry and occupation enforce it.
% TRANSFER_FUNCTION: Annual wealth transfer from German treasury to Allied creditor nations, justified as reparation for war damages but framed by this reading as extraction under duress. The constraint moves capital, industrial capacity (through equipment shipments), and national sovereignty itself — the ability to set own fiscal and defense policy.
% ABSENT_VOICES: The German public was not consulted; they are internal payers. The international legal community, if present, would defend treaty sanctity and warn against unilateral repudiation. Neutral nations and Germany's future victims (those who would be harmed by German rearmament enabled by repudiation) are structurally excluded from the original negotiation.
% DISAPPEARANCE_RATIONALE: If reparations obligations disappeared, Germany would immediately reallocate budget from debt service to military and industrial rebuild, fundamentally altering the European balance of power. Allied economies dependent on reparations inflow would face fiscal pressure and demand new revenue sources. The entire post-war security architecture (occupation forces, disarmament verification, economic leverage) collapses and must be renegotiated or enforced through new mechanisms.
% FOUNDING_PROBLEM: Allied war costs were enormous; Germany was identified as the primary aggressor and defeated militarily. The founding problem framed by the treaty was: how to ensure Germany pays for damages and remains unable to wage aggressive war again? The repudiation reading rejects this framing as victor's justice disguised as legitimate obligation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Allied states and mainstream international legal doctrine, but corroboration from outside the benefiting parties is weak. German legal scholars, economists (Keynes, others outside government), and neutral observers argued the founding problem statement was self-serving: a defeated nation was labeled criminal to justify extraction. Later international law (UN Charter) would formalize duress as voiding consent, corroborating the repudiation reading's claim that the founding problem itself was illegitimate.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__repudiation_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__repudiation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__repudiation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(versailles_reparations_clauses__repudiation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__repudiation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__repudiation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.92 at interval end) because the reparations transfer is decoupled from any genuine coordination benefit and is sustained purely by power asymmetry. Suppression is correspondingly high (0.88) because enforcement depends on occupation forces and the threat of military reoccupation—alternatives (default, repudiation, capital flight) are structurally prevented. Theater is moderate (0.41) because the 'justice' framing of reparations as legitimate punishment gradually erodes as the material devastation of German hyperinflation becomes visible and costs of enforcement mount. The measurement series shows extractiveness spiking in 1923 (peak of reparations crisis and hyperinflation) then stabilizing, while theater rises as more actors openly question the treaty's legitimacy. Suppression requirement peaks in 1923 (when enforcement pressure is highest) and remains elevated throughout, indicating the constraint requires continuous active coercion to persist.
 *
 * PERSPECTIVAL GAP:
 *   From the Allied beneficiary seat: the treaty is legitimate punishment for German aggression, and reparations are justified compensation. From the German payer seats: the treaty is duress-imposed and void, and reparations are extraction under occupation. The engine computes these divergences from the stakeholder directionalities—the repudiation reading explicitly frames the payer experience as correct and the beneficiary frame as illegitimate rhetoric masking power. The claim/metric independence rule applies: the repudiation reading CLAIMS the treaty is a snare (pure extraction), and the authored metrics describe very high extraction, high suppression, and moderate theater—these align, but independently derived facts, not tuned agreement.
 *
 * DIRECTIONALITY LOGIC:
 *   German state and working class are targets (d near 1.0): they pay under duress with no genuine benefit and trapped exit. Allied states are beneficiaries (d near 0.0): they collect extraction with high power and mobile exit (they can end the constraint unilaterally). Weimar leadership sits between (d ~0.6): nominally the agenda-setter but operationally constrained by occupation, so they bear as much cost as they supposedly administer. International legal observers are analytical (d=undefined): they have no structural stake in the constraint itself, only in the principle it rests on.
 *
 * MANDATROPHY ANALYSIS:
 *   The repudiation reading explicitly resolves the mandatrophy question by denying the mandate itself: if the treaty is void because of duress, then the reparations obligation has no legitimate mandate to begin with. This prevents the false positive where extraction is misclassified as 'justified transfer' by appeal to a founding obligation. The reading asserts that the founding problem (German war guilt) was a victor-constructed frame used to justify extraction, and therefore cannot ground legitimacy. The classification as snare follows directly from this denial: pure extraction without a legitimate coordination or justice mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    duress_doctrine_scope,
    'Does the duress doctrine in international law apply to treaties signed under military pressure when the alternative to signing is continued occupation and threat of invasion?',
    'International legal scholarship and precedent (Vienna Convention on the Law of Treaties Article 52, post-WWII legal developments); comparative case law from treaties signed under similar military pressure.',
    'If duress doctrine is accepted as applicable to Versailles, the entire reparations regime is legally void and the repudiation reading is established as legitimate in international law. If duress doctrine is narrowly interpreted as requiring explicit threat of life/bodily harm to signatories (not just military pressure on the state), the reading loses legal grounding and reverts to a political claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(duress_doctrine_scope, empirical, 'Whether military occupation and threat of invasion constitute duress sufficient to void a treaty.').

omega_variable(
    victor_justice_vs_legitimate_punishment,
    'Is the framing of reparations as legitimate punishment for aggression a genuine legal principle, or a victors'' rhetorical cover for extraction of war costs?',
    'Historical analysis of pre-Versailles precedent for reparations clauses; comparative study of how victors in prior wars justified reparations demands; philosophical/legal argument about the distinction between legitimate punishment (by an impartial authority) and victors'' justice (by a belligerent).',
    'If reparations are shown to be post-hoc rhetoric without historical precedent or principled legal grounding, the repudiation reading gains support by exposing the beneficiary framing as constructed. If legitimate punishment doctrine can be demonstrated in prior treaties, the reading loses the claim that the founding problem was purely constructed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victor_justice_vs_legitimate_punishment, conceptual, 'Whether the treaty''s justice framing is a genuine principle or victor''s rationalization.').

omega_variable(
    german_responsibility_ambiguity,
    'What was Germany''s actual causal/moral responsibility for starting the war and its costs, independent of the treaty''s Article 231 determination?',
    'Historical scholarship on July Crisis, German diplomatic/military decisions, comparative analysis of responsibility across all belligerents; post-war historical consensus.',
    'High German responsibility supports the punitive reading and undermines repudiation (the extracted obligation may still be legitimate even if the treaty process was coercive). Shared responsibility across multiple belligerents supports repudiation by showing the treaty''s sole-guilt determination was victor-constructed. This resolves the omega between competing readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(german_responsibility_ambiguity, empirical, 'Whether Article 231''s war guilt attribution is historically accurate or victor-constructed.').

omega_variable(
    reparations_sustainability_ambiguity,
    'Was the reparations amount economically sustainable for Germany without systematic impoverishment, or did the schedule inherently guarantee default/hyperinflation?',
    'Economic historical analysis of German GDP, export capacity, fiscal capacity; counterfactual analysis of sustainable transfer rates; comparison with actual flows and economic outcomes (hyperinflation, default, Dawes Plan restructuring).',
    'If unsustainable, the repudiation reading gains leverage by showing the treaty designed extraction, not legitimate reparation. If sustainable with proper fiscal management, repudiation loses force—the constraint could have been endured if political will existed to service it. The sustainability question distinguishes between extraction (inherently unsustainable, designed to impoverish) and legitimate transfer (difficult but possible).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reparations_sustainability_ambiguity, empirical, 'Whether reparations were economically viable or structurally designed to cause collapse.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.88) structural (occupation forces, military coercion) or internalized (German political acceptance of treaty legitimacy, belief in war guilt)?',
    'Post-treaty historical analysis of German political movements: did repudiation remain a fringe claim or did it gain mainstream support? Did German elites and public internalize treaty legitimacy or resist it? Weimar period shows the reading remaining live and gaining power, suggesting suppression is primarily structural (coercion) not internalized (belief).',
    'If suppression is primarily structural, the constraint is a pure snare requiring continuous enforcement (occupation). If internalized, German resistance would be lower and the constraint more stable. The rise of nationalist movements advocating repudiation suggests suppression is external, not internal—the population rejects the treaty''s legitimacy even under pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether reparations suppression is structural coercion or internalized acceptance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__repudiation_reading, 1919, 1933).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1919, 0.22).
narrative_ontology:measurement_basis(vers_tr_t1919, observed).
narrative_ontology:measurement(vers_tr_t1921, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1921, 0.28).
narrative_ontology:measurement_basis(vers_tr_t1921, observed).
narrative_ontology:measurement(vers_tr_t1923, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1923, 0.35).
narrative_ontology:measurement_basis(vers_tr_t1923, observed).
narrative_ontology:measurement(vers_tr_t1926, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1926, 0.41).
narrative_ontology:measurement_basis(vers_tr_t1926, observed).
narrative_ontology:measurement(vers_tr_t1929, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1929, 0.44).
narrative_ontology:measurement_basis(vers_tr_t1929, observed).
narrative_ontology:measurement(vers_tr_t1933, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1933, 0.41).
narrative_ontology:measurement_basis(vers_tr_t1933, observed).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1919, 0.85).
narrative_ontology:measurement_basis(vers_be_t1919, observed).
narrative_ontology:measurement(vers_be_t1921, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1921, 0.88).
narrative_ontology:measurement_basis(vers_be_t1921, observed).
narrative_ontology:measurement(vers_be_t1923, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1923, 0.94).
narrative_ontology:measurement_basis(vers_be_t1923, observed).
narrative_ontology:measurement(vers_be_t1926, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1926, 0.91).
narrative_ontology:measurement_basis(vers_be_t1926, observed).
narrative_ontology:measurement(vers_be_t1929, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1929, 0.89).
narrative_ontology:measurement_basis(vers_be_t1929, observed).
narrative_ontology:measurement(vers_be_t1933, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1933, 0.92).
narrative_ontology:measurement_basis(vers_be_t1933, observed).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1919, 0.79).
narrative_ontology:measurement_basis(vers_su_t1919, observed).
narrative_ontology:measurement(vers_su_t1921, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1921, 0.82).
narrative_ontology:measurement_basis(vers_su_t1921, observed).
narrative_ontology:measurement(vers_su_t1923, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1923, 0.91).
narrative_ontology:measurement_basis(vers_su_t1923, observed).
narrative_ontology:measurement(vers_su_t1926, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1926, 0.88).
narrative_ontology:measurement_basis(vers_su_t1926, observed).
narrative_ontology:measurement(vers_su_t1929, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1929, 0.85).
narrative_ontology:measurement_basis(vers_su_t1929, observed).
narrative_ontology:measurement(vers_su_t1933, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1933, 0.88).
narrative_ontology:measurement_basis(vers_su_t1933, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__repudiation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(versailles_reparations_clauses__repudiation_reading, 0.08).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__limited_responsibility_reading).

% DUAL FORMULATION NOTE:
% The Versailles reparations clauses constitute a contested kernel decomposed into three constraint stories, each representing a distinct reading with different ε values and stakeholder structures. The repudiation reading instantiated here denies the legitimacy of the treaty entirely (ε=0.92, snare); the punitive reading accepts the treaty as legitimate punishment (ε varies by seat); the limited reading accepts legitimacy but bounds obligation by capacity. These are not perspectives on one constraint—they are three distinct constraints with three distinct ε values, each grounded in a different theory of treaty validity and reparations obligation. Family links via network.affects_constraints document the causal dependency: the repudiation reading directly challenges the authority of the punitive reading by attacking the treaty foundation; the limited reading attempts a compromise that the repudiation reading forecloses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
