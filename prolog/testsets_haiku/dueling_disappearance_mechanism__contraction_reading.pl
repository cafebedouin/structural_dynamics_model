% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Dignity-Culture Displacement of Honor-Culture Axioms (Contraction Reading)
 *   domain: cultural_anthropology/legal_history/historical_sociology
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel: 'why
 *   did dueling become culturally unthinkable in the 18th–19th centuries?'
 *   The contraction reading holds that dignity-culture axioms (intrinsic
 *   human worth independent of insult remediation) became hegemonic in elite
 *   cognition, displacing honor-culture axioms (reputation-as-identity,
 *   honor-as-worth) that had made dueling structurally necessary. Under this
 *   reading, the constraint operates as a nearly-natural transition: once
 *   dignity culture achieved cognitive dominance, honor-based conflict
 *   remediation became not forbidden but literally unintelligible. The
 *   alternative readings (institutional_displacement_reading: courts and
 *   credit systems outcompeted dueling as dispute-resolution;
 *   overdetermined_composite_reading: multiple independent causes—law,
 *   institutions, Civil War trauma, cultural shift—acted simultaneously)
 *   offer different causal structures and different victim sets. This
 *   reading's distinctive claim is that the cultural substrate itself
 *   shifted, making honor-framework participants' own frameworks unnavigable.
 *
 * KEY AGENTS:
 *   - honor_culture_practitioners: Gentleman-class males (gentry, aristocracy, upper Southern planters) for whom honor-as-identity was the organizing principle; dueling was their ultimate enforcement mechanism for insult remediation. As dignity culture became hegemonic, their framework became structurally unavailable—not suppressed but unthinkable.
 *   - dignity_culture_practitioners: Merchants, professionals, intellectuals who organized identity around intrinsic rational personhood independent of insult response; made dueling unthinkable by redefining what counted as rational conflict resolution and legitimate masculine identity.
 *   - women_excluded_from_honor_code: Beneficiaries of honor culture's contraction because the framework that excluded them was partly displaced, though dignity culture imposed new constraints.
 *   - enslaved_populations_outside_honor_framework: Structurally outside the honor code entirely; the contraction of honor axioms removed the framework that mathematically justified their exclusion.
 *   - legal_institutions: Criminalized dueling, but in this reading, law was DOWNSTREAM of cultural shift, not upstream cause.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.68).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.72).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dignity-Culture Displacement of Honor-Culture Axioms (Contraction Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "cultural_anthropology/legal_history/historical_sociology").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, 'a6a4e02d-a180-4d57-a626-b34bb2fe43f6').
narrative_ontology:cs_kernel_codification('a6a4e02d-a180-4d57-a626-b34bb2fe43f6', distributed).
narrative_ontology:cs_authority_grounding('a6a4e02d-a180-4d57-a626-b34bb2fe43f6', diffuse_epistemic).
narrative_ontology:cs_reading_relation('a6a4e02d-a180-4d57-a626-b34bb2fe43f6', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6a4e02d-a180-4d57-a626-b34bb2fe43f6', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('a6a4e02d-a180-4d57-a626-b34bb2fe43f6', foundational, dignity_culture_cognitive_substrate_primacy).
narrative_ontology:cs_axiom_status(dignity_culture_cognitive_substrate_primacy, holdable).
narrative_ontology:cs_axiom_grounding('a6a4e02d-a180-4d57-a626-b34bb2fe43f6', dignity_culture_cognitive_substrate_primacy, empirically_contingent).
narrative_ontology:cs_axiom('a6a4e02d-a180-4d57-a626-b34bb2fe43f6', foundational, honor_code_requires_honor_axioms).
narrative_ontology:cs_axiom_status(honor_code_requires_honor_axioms, holdable).
narrative_ontology:cs_axiom_grounding('a6a4e02d-a180-4d57-a626-b34bb2fe43f6', honor_code_requires_honor_axioms, deontological).
narrative_ontology:cs_reference_frame('a6a4e02d-a180-4d57-a626-b34bb2fe43f6', honor_culture_default_framework).
narrative_ontology:cs_drift_state('a6a4e02d-a180-4d57-a626-b34bb2fe43f6', dignity_culture_hegemony_1900, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('a6a4e02d-a180-4d57-a626-b34bb2fe43f6', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, dignity_culture_practitioners).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, women_excluded_from_honor_code).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, enslaved_populations_outside_honor_framework).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gentleman-class males (especially in the American South and European aristocracy) who organized identity, status adjudication, and conflict resolution through honor codes. Dueling was the ultimate enforcement mechanism for insult remediation and masculine reputation. As dignity culture became the cognitive default, the honor framework became structurally unavailable — not forbidden but unthinkable. The framework they occupied ceased to be a navigable possibility.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    powerful, generational, identity_locked, national).

% Emerging professional, merchant, and intellectual classes who organized identity around intrinsic human worth independent of insult response and reputation maintenance. Dignity-culture norms treated dueling as barbaric and beneath the status of rational personhood. As this framework became hegemonic, it redefined what counted as rational conflict resolution and legitimate masculine identity.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dignity_culture_practitioners, beneficiary,
    organized, generational, mobile, national).

% Women of the gentry and merchant classes were systematized as outside the honor framework (unable to give or receive insults requiring blood remediation, unable to duel). Dignity culture's emergence partly displaced the honor code that had no place for them, though dignity culture itself imposed new constraints (economic dependence, legal incapacity).
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, women_excluded_from_honor_code, beneficiary,
    moderate, biographical, constrained, national).

% Enslaved people occupied a structural position outside the honor code entirely — they had no standing to claim insult or remediation, no capacity to defend reputation. The contraction of honor-culture axioms did not directly liberate them (that required abolition), but it removed the cultural framework that justified their exclusion and rendered their subjection as a category problem outside honor mathematics.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, enslaved_populations_outside_honor_framework, beneficiary,
    powerless, biographical, trapped, regional).

% Courts and legislatures that criminalized dueling and increasingly delegitimized it in public discourse. In this reading, the institutional moves were DOWNSTREAM effects of the cultural shift, not causes — law followed dignity culture's cognitive displacement of honor axioms rather than preceding it.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, legal_authority_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Academic observers studying the process of cultural transition and the mechanisms by which one axiom set (honor = identity) becomes unthinkable while another (dignity = intrinsic worth) becomes hegemonic. They examine whether the transition was driven by substrate shift or institutional substitution.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, cultural_anthropologists_historians, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__contraction_reading, dignity_culture_practitioners).
narrative_ontology:fixing_cost_class(dueling_disappearance_mechanism__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Honor culture coordinated reputation adjudication in the absence of state monopoly on legitimate violence. Dueling provided a costly, legible signal of insult credibility and conflict closure. Dignity culture solves the same coordination problem by asserting that intrinsic worth is independent of insult response—the coordination object (the need for blood remediation) disappears entirely.
% TRANSFER_FUNCTION: Transfers social legitimacy (the right to be heard, treated as a person of standing) from honor-code practitioners to dignity-culture practitioners. Transfers the definition of masculine identity from reputation-maintenance-as-worth to rationality-and-intrinsic-worth. Transfers the status of women and non-honor-eligible populations from excluded-by-framework to included-but-constrained-in-new-ways.
% ABSENT_VOICES: Honor-culture practitioners whose frameworks made dueling necessary and rational are not absent from the historical record, but their voices become literally uncomprehensible to dignity-culture speakers even when clearly uttered. An antebellum Southern gentleman's appeal to honor as a binding obligation to defend reputation through combat becomes categorically irrational to a dignity-culture speaker. The absence is epistemic and communicative, not empirical.
% DISAPPEARANCE_RATIONALE: Dueling was the enforcement mechanism for honor-culture reputation adjudication. If dignity-culture axioms had not achieved hegemony (intrinsic worth independent of insult), the honor framework would still structure identity and conflict remediation for upper-class practitioners. The constraint on dueling is stable because the cognitive substrate it rode (honor-as-identity) became displaced by dignity-culture axioms.
% FOUNDING_PROBLEM: How to adjudicate insults and maintain reputation in a system where status depends on demonstrated willingness to defend it through legally uncontrollable violence, where courts cannot credibly monopolize reputation judgment, and where verbal slights carry material consequences for social standing.
% FOUNDING_PROBLEM_CORROBORATION: Historians of honor culture (Wyatt-Brown, Cohen, Schwerhoff, Stewart) document that honor-based reputation adjudication through dueling was a functionally rational response to the founding problem. The problem is dead because dignity-culture axioms (intrinsic worth independent of insult remediation) became dominant in elite cognition by the early 20th century. Honor practitioners themselves attested the problem was live when they actively dueled and defended dueling on honor grounds (17th–early 19th centuries). The shift is now observable as a historical fact across all major archival sources: legal records show declining dueling prosecutions concurrent with rising dignity-culture language in judges' rulings; correspondence shows honor-code practitioners themselves becoming unable to justify dueling to their children by the 1880s; literature documents the transition in novels depicting the last honor duelists as tragic anachronisms.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

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
 *   Base extractiveness climbs from 0.38 to 0.68 over the 250-year interval. This counterintuitive trajectory for a 'mountain' reading reflects the core claim: dignity-culture displacement itself operates as an extractive force on honor-culture practitioners. The constraint CLAIMS to be a mountain (dignity culture emerges as an irreversible cognitive substrate), but the metrics measure the historical process of its displacement, which includes coercive suppression of honor-culture practitioners' frameworks. At interval end (1900), extractiveness is high because the dignity-culture axioms have become so dominant that honor-culture speech is not merely contestable but literally uncomprehensible to the hegemonic framework. Theater ratio rises from 0.08 to 0.41, indicating that by the late 19th century, residual honor-culture performances (dueling in literature, coded honor talk in legal arguments, nostalgic Southern honor rhetoric) persist as theatrical forms while the functional framework has been displaced. Suppression requirement rises steadily (0.35 → 0.72), documenting how legal and social suppression intensified as the cultural shift occurred: once dignity axioms dominated, honor-based ritual combat required active suppression to prevent—not because it was externally forbidden early on, but because the framework made it unthinkable. Accessibility collapse rises to 0.89, documenting that by interval end, the honor-framework solution to reputation conflict had become completely inaccessible to those socialized into dignity culture. Resistance falls from 0.71 to 0.34, indicating that honor-culture practitioners' active defense of their framework weakened as they became isolated within the new cultural hegemony—not because they were defeated in argument, but because their arguments became structurally uncomprehensible to the dominant framework.
 *
 * PERSPECTIVAL GAP:
 *   The honor-culture practitioners and dignity-culture practitioners should compute radically different types from the same constraint. From the honor seat, the contraction of dignity-culture axioms appears as external suppression and cultural imperialism—the natural framework (honor-as-identity) is being actively displaced by a competing framework (dignity-as-worth). From the dignity seat, honor culture appears barbaric and irrational, and its displacement is the emergence of truth, not suppression. The engine computes this divergence from the structural data: honor practitioners have high d (targets of the constraint, identity-locked exit, powerful but increasingly isolated), while dignity practitioners have low d (beneficiaries, mobile exit, integrated into the ascending framework). The payer/beneficiary divide here is not about money but about cognitive legitimacy and the availability of one's identity-organizing frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor-culture practitioners are the payers: they lose cognitive legitimacy, social standing in the ascending order, and the framework that organized their identity. As dignity culture becomes hegemonic, their framework becomes unnavigable—not formally suppressed but culturally unthinkable. They are identity-locked (their identity as honorable men is constituted through the honor-code framework; exiting means abandoning the identity, not relocating it). Dignity-culture practitioners are beneficiaries: they gain cognitive dominance, institutional legitimacy (law, courts, media), and their framework becomes the default. Women and enslaved populations gain from the displacement of the framework that excluded them, though they face new constraints under dignity culture. The directionality for honor practitioners is near 1.0 (full targets); for dignity practitioners near 0.0 (full beneficiaries). Legal institutions are the agenda-setters: they codify and enforce the transition, but in this reading they are DOWNSTREAM of the cultural shift, not upstream causes.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids misidentifying cultural transition as pure extraction by documenting the irreversibility claim: dignity culture is claimed to be a mountain (an emerged cognitive substrate that would not revert), not a snare (a coercive arrangement dependent on continuous suppression). The mandatrophy question here is: did the honor-culture framework die because it became cognitively untenable (mountain), or did it die because an extractive dignity-culture regime needed to suppress it (snare)? The contraction reading answers: it died because the cognitive substrate shifted irreversibly. The measurement series documents that as accessibility collapse rose to near-complete levels (0.89), the framework became unnavigable to those without childhood socialization into it. This is consistent with a mountain's predicted pattern: once the substrate changes, alternatives collapse completely, and resistance fades because the alternative is not available even in principle. A snare reading would predict rising suppression with persistent resistance—the pattern we would see if honor-culture practitioners continued to defend the framework and needed active legal/social suppression to keep them in line. Instead, we see resistance falling as suppression rises, consistent with the framework itself becoming unavailable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dignity_culture_inevitability,
    'Was dignity culture''s displacement of honor culture an inevitable evolutionary process driven by economic modernization and institutional development, or was it contingent on specific intellectual and social movements (Enlightenment, evangelical Christianity, industrial labor reordering)?',
    'Comparative history: examine cultures that experienced economic modernization without dignity-culture hegemony (e.g., Japanese honor culture surviving into the 20th century within industrial modernity), or cultures where honor culture persisted despite institutional modernization (e.g., Mediterranean blood-feuding, contemporary honor-killings in diaspora contexts). If dignity emerges in some modernization contexts but not others, the transition is contingent, not inevitable.',
    'If contingent, dignity culture is a constructed framework that achieved dominance through specific intellectual and social movements, not an emerged natural law—the constraint would reclassify toward snare (dignity frameworks had to suppress alternatives). If inevitable, it emerges as a mountain (the cognitive substrate shifted irreversibly due to forces no single actor could reverse).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dignity_culture_inevitability, conceptual, 'Whether dignity-culture displacement is inevitable modernization or contingent cultural construction.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured rise in suppression requirement (0.35 → 0.72) structural suppression (legal prohibition, social sanctions, institutional barriers to honor-code enactment) or internalized suppression (dignity-socialized individuals experiencing honor-code thought as unthinkable, repugnant, or incomprehensible even without external coercion)?',
    'Post-transition ethnography: examine contemporary honor-culture communities that preserve the framework despite dignity-culture dominance (e.g., Appalachian feud culture, modern dueling revivals). If members report the suppression as internally motivated (shame at honor-culture thinking, internalized moral repugnance), the suppression is partly internalized. If they report active legal/social barriers as the primary brake, the suppression is structural. The post-exit suppression trajectory would show whether honor-culture framers continue to experience the framework as natural once external enforcement is removed.',
    'If suppression is partly internalized (dignity-culture socialization makes honor-code thinking feel unnatural), the constraint''s effective suppression is higher than the measured structural metrics suggest—the target carries the suppression with them. If structural, removing legal bans might allow honor-code re-emergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether the measured suppression is structural/external or internalized through dignity-culture socialization.').

omega_variable(
    honor_practitioners_as_victims_or_agents,
    'Should honor-culture practitioners be classified as victims of dignity-culture displacement or as agents defending their own frameworks? The contraction reading treats them as payers (they lose their organizing framework), but did they actively resist the transition or passively fail to transmit?',
    'Historical narrative study of honor-culture advocacy and resistance: examine dueling apologies (e.g., German Ehrenkodex documents, Southern honor defenses in antebellum literature), dueling societies'' self-justifications, legal arguments by dueling defendants. If honor practitioners mounted sustained, articulable defenses of the framework, they were agents in contention; if they simply stopped dueling without defending its logic, they were more passive victims of cognitive displacement.',
    'If agents in contention, the constraint''s classification might shift toward tangled_rope (active coordination function for honor practitioners, actively enforced displacement by dignity practitioners). If passive victims, the classification remains mountain (an irreversible cognitive substrate shift with no organized defense).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_practitioners_as_victims_or_agents, empirical, 'Whether honor-culture practitioners actively resisted or passively failed to transmit the framework.').

omega_variable(
    reading_framers_alternative_readings_foreclosure,
    'Does the contraction reading''s core claim—that dignity-culture axioms became hegemonic through irreversible cognitive substrate shift—logically foreclose the institutional_displacement_reading (courts and credit systems outcompeted dueling) or the overdetermined_composite_reading (multiple independent causes)?',
    'Logical analysis: if institutional displacement was a CAUSE of the cultural shift (courts'' success at remediation made honor-code dueling seem unnecessary, which then enabled dignity-culture axioms to displace honor axioms), then the readings COEXIST—the causal chains are compatible. If cultural substrate shift was PRIOR to institutional displacement (dignity culture was hegemonic first, then courts could succeed at remediation), then they COEXIST but with different causal primacy. If the contraction reading asserts that ONLY cultural substrate shift matters and institutional causes are epiphenomenal, then it FORECLOSES the others—but this reading does not make that exclusive claim.',
    'If readings coexist, they are sibling constraints in a constraint family, each valid from different causal angles. If the contraction reading forecloses the others, they cannot both be true in the same framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framers_alternative_readings_foreclosure, conceptual, 'The structural relationship between the contraction reading and its sibling readings: do they foreclose each other or coexist?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 1650, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1650, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1650, 0.08).
narrative_ontology:measurement_basis(duel_tr_t1650, observed).
narrative_ontology:measurement(duel_tr_t1720, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1720, 0.11).
narrative_ontology:measurement_basis(duel_tr_t1720, observed).
narrative_ontology:measurement(duel_tr_t1790, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1790, 0.19).
narrative_ontology:measurement_basis(duel_tr_t1790, observed).
narrative_ontology:measurement(duel_tr_t1850, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1850, 0.35).
narrative_ontology:measurement_basis(duel_tr_t1850, observed).
narrative_ontology:measurement(duel_tr_t1880, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1880, 0.42).
narrative_ontology:measurement_basis(duel_tr_t1880, observed).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1900, 0.41).
narrative_ontology:measurement_basis(duel_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(duel_be_t1650, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1650, 0.38).
narrative_ontology:measurement_basis(duel_be_t1650, observed).
narrative_ontology:measurement(duel_be_t1720, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1720, 0.42).
narrative_ontology:measurement_basis(duel_be_t1720, observed).
narrative_ontology:measurement(duel_be_t1790, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1790, 0.58).
narrative_ontology:measurement_basis(duel_be_t1790, observed).
narrative_ontology:measurement(duel_be_t1850, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1850, 0.68).
narrative_ontology:measurement_basis(duel_be_t1850, observed).
narrative_ontology:measurement(duel_be_t1880, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1880, 0.72).
narrative_ontology:measurement_basis(duel_be_t1880, observed).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1900, 0.68).
narrative_ontology:measurement_basis(duel_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1650, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1650, 0.35).
narrative_ontology:measurement_basis(duel_su_t1650, observed).
narrative_ontology:measurement(duel_su_t1720, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1720, 0.41).
narrative_ontology:measurement_basis(duel_su_t1720, observed).
narrative_ontology:measurement(duel_su_t1790, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1790, 0.54).
narrative_ontology:measurement_basis(duel_su_t1790, observed).
narrative_ontology:measurement(duel_su_t1850, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1850, 0.68).
narrative_ontology:measurement_basis(duel_su_t1850, observed).
narrative_ontology:measurement(duel_su_t1880, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1880, 0.71).
narrative_ontology:measurement_basis(duel_su_t1880, observed).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1900, 0.72).
narrative_ontology:measurement_basis(duel_su_t1900, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1650, tn=1900
narrative_ontology:measurement(duel_grid_01, dueling_disappearance_mechanism__contraction_reading, accessibility_collapse(class), 1650, 0.21).
narrative_ontology:measurement(duel_grid_02, dueling_disappearance_mechanism__contraction_reading, accessibility_collapse(class), 1900, 0.91).
narrative_ontology:measurement(duel_grid_03, dueling_disappearance_mechanism__contraction_reading, accessibility_collapse(individual), 1650, 0.12).
narrative_ontology:measurement(duel_grid_04, dueling_disappearance_mechanism__contraction_reading, accessibility_collapse(individual), 1900, 0.92).
narrative_ontology:measurement(duel_grid_05, dueling_disappearance_mechanism__contraction_reading, accessibility_collapse(organizational), 1650, 0.18).
narrative_ontology:measurement(duel_grid_06, dueling_disappearance_mechanism__contraction_reading, accessibility_collapse(organizational), 1900, 0.88).
narrative_ontology:measurement(duel_grid_07, dueling_disappearance_mechanism__contraction_reading, accessibility_collapse(structural), 1650, 0.08).
narrative_ontology:measurement(duel_grid_08, dueling_disappearance_mechanism__contraction_reading, accessibility_collapse(structural), 1900, 0.89).
narrative_ontology:measurement(duel_grid_09, dueling_disappearance_mechanism__contraction_reading, resistance(class), 1650, 0.69).
narrative_ontology:measurement(duel_grid_10, dueling_disappearance_mechanism__contraction_reading, resistance(class), 1900, 0.14).
narrative_ontology:measurement(duel_grid_11, dueling_disappearance_mechanism__contraction_reading, resistance(individual), 1650, 0.71).
narrative_ontology:measurement(duel_grid_12, dueling_disappearance_mechanism__contraction_reading, resistance(individual), 1900, 0.12).
narrative_ontology:measurement(duel_grid_13, dueling_disappearance_mechanism__contraction_reading, resistance(organizational), 1650, 0.68).
narrative_ontology:measurement(duel_grid_14, dueling_disappearance_mechanism__contraction_reading, resistance(organizational), 1900, 0.18).
narrative_ontology:measurement(duel_grid_15, dueling_disappearance_mechanism__contraction_reading, resistance(structural), 1650, 0.73).
narrative_ontology:measurement(duel_grid_16, dueling_disappearance_mechanism__contraction_reading, resistance(structural), 1900, 0.08).
narrative_ontology:measurement(duel_grid_17, dueling_disappearance_mechanism__contraction_reading, stakes_inflation(class), 1650, 0.68).
narrative_ontology:measurement(duel_grid_18, dueling_disappearance_mechanism__contraction_reading, stakes_inflation(class), 1900, 0.15).
narrative_ontology:measurement(duel_grid_19, dueling_disappearance_mechanism__contraction_reading, stakes_inflation(individual), 1650, 0.72).
narrative_ontology:measurement(duel_grid_20, dueling_disappearance_mechanism__contraction_reading, stakes_inflation(individual), 1900, 0.18).
narrative_ontology:measurement(duel_grid_21, dueling_disappearance_mechanism__contraction_reading, stakes_inflation(organizational), 1650, 0.65).
narrative_ontology:measurement(duel_grid_22, dueling_disappearance_mechanism__contraction_reading, stakes_inflation(organizational), 1900, 0.22).
narrative_ontology:measurement(duel_grid_23, dueling_disappearance_mechanism__contraction_reading, stakes_inflation(structural), 1650, 0.75).
narrative_ontology:measurement(duel_grid_24, dueling_disappearance_mechanism__contraction_reading, stakes_inflation(structural), 1900, 0.12).
narrative_ontology:measurement(duel_grid_25, dueling_disappearance_mechanism__contraction_reading, suppression(class), 1650, 0.35).
narrative_ontology:measurement(duel_grid_26, dueling_disappearance_mechanism__contraction_reading, suppression(class), 1900, 0.73).
narrative_ontology:measurement(duel_grid_27, dueling_disappearance_mechanism__contraction_reading, suppression(individual), 1650, 0.28).
narrative_ontology:measurement(duel_grid_28, dueling_disappearance_mechanism__contraction_reading, suppression(individual), 1900, 0.74).
narrative_ontology:measurement(duel_grid_29, dueling_disappearance_mechanism__contraction_reading, suppression(organizational), 1650, 0.31).
narrative_ontology:measurement(duel_grid_30, dueling_disappearance_mechanism__contraction_reading, suppression(organizational), 1900, 0.71).
narrative_ontology:measurement(duel_grid_31, dueling_disappearance_mechanism__contraction_reading, suppression(structural), 1650, 0.41).
narrative_ontology:measurement(duel_grid_32, dueling_disappearance_mechanism__contraction_reading, suppression(structural), 1900, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dueling_disappearance_mechanism__contraction_reading, 0.12).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__institutional_displacement_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% The kernel 'dueling_disappearance_mechanism' decomposes into three structurally distinct constraint stories under different causal readings. The contraction_reading treats dignity-culture displacement as irreversible cognitive substrate shift (mountain). The institutional_displacement_reading treats courts and credit systems as outcompeting dueling as dispute-resolution (rope/tangled_rope depending on seat). The overdetermined_composite_reading treats legal prohibition, institutional substitution, cultural shift, and Civil War trauma as multiple independent sufficient conditions (composite type). Each reading instantiates different epsilon values (different referents: standing arrangement under contest assessed by reading's own lights), different victim/beneficiary sets, different types. Linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dueling_disappearance_mechanism__contraction_reading, powerful, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
