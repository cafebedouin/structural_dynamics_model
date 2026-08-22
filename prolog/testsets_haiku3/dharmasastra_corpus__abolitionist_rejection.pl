% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__abolitionist_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__abolitionist_rejection, []).

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
 *   constraint_id: dharmasastra_corpus__abolitionist_rejection
 *   human_readable: Dharmasastra Corpus Authority (Abolitionist Reading)
 *   domain: religious/legal/normative
 *
 * SUMMARY:
 *   The Dharmasastra corpus (Manusmriti and related texts) is a set of
 *   Sanskrit texts prescribing social, legal, and ritual norms for Hindu
 *   societies, primarily from the second to fifth centuries CE but
 *   continuously reinterpreted and reaffirmed through later centuries. The
 *   corpus codifies the varna system (Brahmin, Kshatriya, Vaishya, Shudra)
 *   and jati (caste) hierarchy, assigns occupations and life roles by birth,
 *   prescribes female subordination, and claims these hierarchies reflect
 *   cosmic order (rita). This is ONE READING of a contested kernel — the
 *   abolitionist rejection reading. It holds that the Dharmasastra corpus is
 *   fundamentally a mechanism of oppression, that the varna/jati system has
 *   no legitimate foundation, and that the entire textual framework must be
 *   abandoned rather than reinterpreted. This reading emerged strongly in
 *   nineteenth-century Hindu reform movements, Dalit philosophy (especially
 *   Ambedkar), and contemporary anti-caste movements. The three readings of
 *   the Dharmasastra corpus kernel are: (1) abolitionist_rejection (this
 *   constraint) — the corpus is oppressive and must be wholly abandoned; (2)
 *   orthodox_literalist — the corpus is revealed truth, varna is eternal,
 *   caste obligations are binding dharma; (3) reformist_contextual — the
 *   corpus reflects historical conditions, the ethical core can be separated
 *   from time-bound caste prescriptions and preserved in a modern Hindu
 *   framework. Each instantiates a structurally distinct constraint with
 *   different victim sets, beneficiary structures, and ε values.
 *
 * KEY AGENTS:
 *   - Brahminical elite (Brahmin scholars, priests): agenda-setters who monopolize textual interpretation and defend corpus authority; powerful institutional position dependent on uncontested textual supremacy.
 *   - Upper castes (Kshatriya, Vaishya): beneficiaries who collect economic and social privilege justified by varna hierarchy; powerful structural position in occupation and property control.
 *   - Shudras: victims prescribed as servants; trapped in occupational restriction and ritual subordination by birth.
 *   - Dalits/Untouchables: victims placed outside or below varna system; subjected to ritual pollution, denial of temple access, forced occupation in polluting labor; most severely oppressed tier.
 *   - Women (all castes): victims experiencing gendered subordination across all varnas; identity_locked into wifehood and dependency via cultural fusion of female identity with subordination.
 *   - Colonized Hindu population (observer): organized under colonial rule; increasingly read the corpus as obstacle to national sovereignty and democratic equality; positioned as the site where competing readings (orthodox defense, reformist compromise, abolitionist rejection) play out.
 *   - Brahminical textual authority (non-agent proposition): the vindicated doctrine that Brahminical interpretation is supreme and self-evident; rejected by the abolitionist reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, 0.92).
domain_priors:suppression_score(dharmasastra_corpus__abolitionist_rejection, 0.88).
domain_priors:theater_ratio(dharmasastra_corpus__abolitionist_rejection, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, extractiveness, 0.92).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__abolitionist_rejection, snare).
narrative_ontology:human_readable(dharmasastra_corpus__abolitionist_rejection, "Dharmasastra Corpus Authority (Abolitionist Reading)").
narrative_ontology:topic_domain(dharmasastra_corpus__abolitionist_rejection, "religious/legal/normative").

domain_priors:requires_active_enforcement(dharmasastra_corpus__abolitionist_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__abolitionist_rejection, '93de67f4-ce99-49e5-8923-4b3d4c2cbb21').
narrative_ontology:cs_kernel_codification('93de67f4-ce99-49e5-8923-4b3d4c2cbb21', fixed_text).
narrative_ontology:cs_authority_grounding('93de67f4-ce99-49e5-8923-4b3d4c2cbb21', extraction).
narrative_ontology:cs_interpretation_layer_present('93de67f4-ce99-49e5-8923-4b3d4c2cbb21').
narrative_ontology:cs_reading_relation('93de67f4-ce99-49e5-8923-4b3d4c2cbb21', dharmasastra_corpus__orthodox_literalist, forecloses).
narrative_ontology:cs_reading_relation('93de67f4-ce99-49e5-8923-4b3d4c2cbb21', dharmasastra_corpus__reformist_contextual, influences).
narrative_ontology:cs_axiom('93de67f4-ce99-49e5-8923-4b3d4c2cbb21', foundational, textual_authority_is_coercive_not_revealed).
narrative_ontology:cs_axiom_status(textual_authority_is_coercive_not_revealed, holdable).
narrative_ontology:cs_axiom_grounding('93de67f4-ce99-49e5-8923-4b3d4c2cbb21', textual_authority_is_coercive_not_revealed, deontological).
narrative_ontology:cs_axiom('93de67f4-ce99-49e5-8923-4b3d4c2cbb21', foundational, hierarchy_cannot_be_separated_from_core).
narrative_ontology:cs_axiom_status(hierarchy_cannot_be_separated_from_core, holdable).
narrative_ontology:cs_axiom_grounding('93de67f4-ce99-49e5-8923-4b3d4c2cbb21', hierarchy_cannot_be_separated_from_core, deontological).
narrative_ontology:cs_reference_frame('93de67f4-ce99-49e5-8923-4b3d4c2cbb21', universal_human_equality_and_dignity).
narrative_ontology:cs_drift_state('93de67f4-ce99-49e5-8923-4b3d4c2cbb21', colonial_and_postcolonial_india, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('93de67f4-ce99-49e5-8923-4b3d4c2cbb21', '2026-06-12T14:23:45Z').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, brahminical_elite).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, upper_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, shudras).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, dalits_untouchables).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, women_all_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, colonized_hindu_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Brahminical scholars and priestly class interpret, transmit, and enforce Dharmasastra corpus as authoritative textual truth. They monopolize ritual knowledge, claim exclusive authority to adjudicate dharma, perform gatekeeping on textual interpretation, and defend the varna hierarchy as divinely ordained. Their institutional power and status depend on the corpus remaining uncontested and literal.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, brahminical_elite, agenda_setter,
    institutional, civilizational, arbitrage, regional).

% Kshatriya and Vaishya castes benefit from prescribed social and economic privileges, control of property and political authority justified by varna hierarchy. They enforce caste endogamy and occupational restriction on lower castes to maintain economic monopolies and social distance.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, upper_castes, beneficiary,
    powerful, civilizational, arbitrage, regional).

% Shudras are constitutively prescribed as servants (seva) to the three upper varnas. Dharmasastra denies them Vedic study rights, restricts occupational mobility, mandates obedience to upper castes, and justifies their exclusion from ritual authority and property ownership. Their prescribed role is submission without reciprocal obligation to rulers.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, shudras, payer,
    powerless, civilizational, trapped, regional).

% Placed ritually outside the varna system entirely or at the bottom tier, performing occupations designated polluting (leather work, sanitation, corpse handling). Subjected to ritual prohibition from temples, wells, shared spaces; denied education, ritual participation, and legal protection. The corpus codifies their marginalization as natural/cosmic order.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dalits_untouchables, payer,
    powerless, civilizational, trapped, regional).

% Dharmasastra prescribes female subordination across all varnas: dependency on father, then husband, then son; denial of independent property rights, Vedic study, and inheritance; requirement of pativrata (wifehood as dharma). Their identity as woman is fused with subordination such that exit means abandoning gender identity itself within the cultural frame.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, women_all_castes, payer,
    powerless, civilizational, identity_locked, regional).

% Hindu populations under colonial rule experience Dharmasastra as a constraint on collective sovereignty and modernization. Colonial authorities reify the caste system via legal codification and census categories, embedding the corpus's hierarchy into colonial law. Anti-caste reform movements and independence movements increasingly read the corpus as an obstacle to national unity and democratic equality.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, colonized_hindu_population, observer,
    organized, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__abolitionist_rejection, brahminical_elite).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__abolitionist_rejection, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Dharmasastra corpus claims to coordinate social order by prescribing roles (varna), occupations (jati), life stages (ashrama), and obligations (svadharma). It presents hierarchy as natural division of labor grounded in cosmic order.
% TRANSFER_FUNCTION: Moves labor, obedience, and ritual deference upward from lower castes and women to upper castes and men; moves economic monopoly on ritual knowledge, land, and political authority to Brahminical and upper-caste elites. The arrangement transfers human dignity, autonomy, and life-chance downward.
% ABSENT_VOICES: Shudras, Dalits, and women are not in the interpretive conversation — they are the objects of the corpus's prescriptions, not its agents. They would (and do, in reform and abolitionist movements) argue that the hierarchy has no justification, that alternatives (equality, merit-based occupation, gender equity) are possible and superior, and that the corpus's authority is backed by force, not truth.
% DISAPPEARANCE_RATIONALE: If Dharmasastra authority and the varna hierarchy enforced by it vanished overnight, Hindu social organization would restructure: occupations would open by merit not birth, intermarriage would become possible, temple access and ritual participation would become universal, women would have property rights and choice in marriage, and Dalit communities would exit ritually prescribed polluting labor. The entire hierarchy of honor and obligation would collapse. Colonial and post-colonial reorganization around legal equality would accelerate.
% FOUNDING_PROBLEM: Early Vedic society faced coordination around agricultural labor division, ritual specialization, and social stability in a hierarchical, agrarian economy. The Dharmasastra texts were composed to codify and defend such hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: The abolitionist reading holds the founding problem is dead: modern economies operate by merit-based occupation, legal equality, and democratic participation — not caste hierarchy. This reading is corroborated by reform movements within Hinduism (nineteenth-century Brahmo Samaj, contemporary Dalit movements), by constitutional rejection of caste discrimination in post-independence India, by the impossibility of enforcing occupational restrictions and ritual prohibition under modern legal systems, and by the documented harm of caste hierarchy (health disparities, violence, educational exclusion). The orthodox and reformist readings contest this verdict by claiming the founding problem (dharmic order, social stability) remains live and that the corpus provides wisdom even if specific prescriptions are outdated.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__abolitionist_rejection, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__abolitionist_rejection, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__abolitionist_rejection, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dharmasastra_corpus__abolitionist_rejection, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__abolitionist_rejection, 0.92, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__abolitionist_rejection_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__abolitionist_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.92) because the corpus's entire structure redistributes human dignity, autonomy, and material resources upward to upper-caste males and downward to Shudras, Dalits, and women. The extraction is not a side effect of coordination — it is the primary function. Suppression is extremely high (0.88) because the corpus maintains its grip by (1) restricting Vedic education to Brahmins, preventing lower castes from learning the texts themselves; (2) prescribing ritual pollution for Dalits, creating isolation and disgust-based barriers; (3) fusing female identity with subordination via concepts of pativrata and swadharma; (4) enforcing occupational restriction by birth, trapping lower castes in prescribed roles. The combination of physical restriction (separation, ritual prohibition), informational restriction (denied textual access), and identity restriction (shame, belief in natural order) creates a multi-layered suppression system. Theater ratio is moderate (0.41) because while genuine ritual and dharmic functions exist, a substantial and growing share of the corpus's maintenance effort in the period measured goes to defending caste hierarchy and female subordination against reform challenge, not to performing the coordination function. The accessibility_collapse (0.78) reflects that once the caste hierarchy is internalized and enacted, alternatives (merit-based occupation, gender equality, inter-caste marriage) appear impossible or sinful to those socialized within it. Resistance (0.72) is substantial because reform and abolitionist movements actively challenge the corpus's authority, especially from the nineteenth century onward and in post-independence India. The abolitionist reading itself is the expression of this resistance. The measurement series show extractiveness and suppression reaching a plateau over the interval (0.92 by midpoint), reflecting the corpus's stability despite increasing resistance — the constraint hardens its enforcement to resist challenge rather than adapting.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and upper-caste beneficiary seats would compute the constraint as coordination (natural social order, dharmic duty, ritual necessity) with modest overhead. The victim seats (shudras, dalits, women) would compute it as pure extraction with negligible coordination benefit — the 'order' being coordinated is the order of their oppression. The colonized_hindu_population observer seat would compute it as increasingly indefensible institutional inertia that blocks democratic reorganization. The abolitionist reading asserts that the victim seats' computation is structurally correct — there is no coordination benefit that justifies the hierarchy; it is pure extraction dressed in philosophical language. The reformist reading would agree about the oppression but argue a coordination core (social stability, dharmic ethics) can be salvaged by jettisoning the caste prescriptions. The orthodox reading would deny the oppression frame entirely, arguing the hierarchy reflects natural/cosmic order and benefits all by maintaining dharmic order.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahminical elite and upper castes have directionality near 0.0 (full beneficiaries) — they are explicitly benefited by the corpus's enforcement and face no exit cost (they can exit the constraint by simply ignoring it; enforcement falls on those below them). Shudras and Dalits have directionality near 1.0 (full targets) — they are explicitly named as victims in the texts themselves ('the Shudra's duty is service to the three twice-born'; Manusmriti 1.91), face the highest suppression and exit cost (occupational and residential restriction, ritual exclusion), and have no material benefit from the constraint. Women across all castes have directionality near 0.95 (near-full target), with the addition of identity_locked exit: the cultural fusion of female identity with subordination to male authority means exit would require abandoning gender identity itself within the cultural framework, making the effective exit cost even higher than for Shudras (who could theoretically change occupation or location, though massive barriers exist). The abolitionist reading's structural claim is that this directionality distribution IS the entire point of the corpus — it is designed to create and maintain a hierarchy where some castes and all women are fully extracted from by others. No beneficiary/victim mix justifies this extraction; alternatives (equality, merit-based occupation, gender parity) are possible and superior.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordination of agrarian labor, ritual specialization, social stability in ancient agrarian hierarchy) is arguably dead or substantially transformed. Modern Hindu societies operate through legal equality, merit-based occupation, democratic governance, and market economies — not caste role prescription. The abolitionist reading asserts that the corpus's mandate has outlived its function, if it ever had a legitimate one. The reformist reading argues the mandate is still live (dharmic order, spiritual wisdom) but can be preserved after discarding the caste prescriptions. The orthodox reading claims the mandate is eternally live — the hierarchy reflects cosmic order, and abandoning it is spiritual catastrophe. The tension between these readings is precisely the mandatrophy question: has the founding problem been solved by other means, or is it still live? The abolitionist reading resolves this by arguing (1) the founding problem was never legitimate to begin with (coordination for hierarchy is not a valid justification); and (2) the actual function of the corpus is not coordination but extraction disguised as coordination. Therefore, mandatrophy resolution does not apply — the corpus is not a degraded rope or scaffold, but a snare whose function was extraction all along.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the Dharmasastra corpus''s legitimacy grounded in revealed truth (as the orthodox reading holds) or in contingent historical authority (as both the reformist and abolitionist readings hold)?',
    'This is a conceptual/theological question without empirical resolution within a single epistemological framework. Different Hindu communities resolve it differently via textual hermeneutics, appeals to experience, and normative argument. The abolitionist reading asserts the corpus cannot be salvaged by reinterpretation — the hierarchy itself is the problem, not the interpretation.',
    'Orthodox reading: corpus remains authoritative; reinterpretation is heresy. Reformist reading: ethical core can be preserved after jettisoning caste prescriptions. Abolitionist reading: the entire textual framework must be abandoned; no redeemable core exists. Each reading produces a different constraint with different ε and victim sets.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The three readings of Dharmasastra corpus differ fundamentally on whether textual authority is revealed (eternal) or historical (contingent).').

omega_variable(
    suppression_internalization_mechanism,
    'To what extent has the caste hierarchy become internalized into self-concept (shame, ritual purity beliefs, occupational identity) versus remaining a purely structural/external coercion?',
    'Post-exit trajectory analysis: if individuals leaving caste communities (via conversion, migration, identity reform) report persistence of internalized shame and occupational anxiety after external enforcement disappears, that indicates internalization. If exit rapidly produces freedom from these cognitions, suppression is primarily structural.',
    'If highly internalized, the constraint''s effective suppression persists even after institutional enforcement collapses, requiring additional intervention (consciousness-raising, community formation, identity reconstruction). If structural, liberation is achievable through institutional change alone. The abolitionist reading assumes some internalization but argues the solution is dismantling the entire framework rather than reinterpreting it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether suppression under caste hierarchy is structural (external) or internalized (psychological), and to what degree each.').

omega_variable(
    alternative_hindu_coordination_possible,
    'Can a Hindu religious and social framework coordinate community without caste hierarchy? Is caste hierarchy structurally necessary for Hindu dharmic order, or is it contingent?',
    'Existence proof: modern Hindu reform communities, secular movements, and Dalit-led Hindu traditions (e.g., Ambedkar''s Navayana Buddhism emerging from Hindu context) show coordination without varna hierarchy. This supports the abolitionist claim that the hierarchy is contingent, not necessary.',
    'If caste hierarchy is contingent, the abolitionist claim that it can be wholly abandoned without sacrificing dharma itself (understood as righteous conduct, social ethics, spiritual practice) is structurally sound. If necessary, the reformist compromise (preserving ethical core, jettisoning caste) might be the only viable path. Orthodoxy argues the hierarchy is necessary and any abandonment destroys dharma itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_hindu_coordination_possible, empirical, 'Whether caste hierarchy is logically necessary for Hindu coordination or structurally contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__abolitionist_rejection, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dhar_tr_t3, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 3, 0.3).
narrative_ontology:measurement(dhar_tr_t8, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 8, 0.35).
narrative_ontology:measurement(dhar_tr_t13, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 13, 0.4).
narrative_ontology:measurement(dhar_tr_t18, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 18, 0.41).
narrative_ontology:measurement(dhar_tr_t25, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 0, 0.88).
narrative_ontology:measurement(dhar_be_t3, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 3, 0.9).
narrative_ontology:measurement(dhar_be_t8, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 8, 0.91).
narrative_ontology:measurement(dhar_be_t13, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 13, 0.92).
narrative_ontology:measurement(dhar_be_t18, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 18, 0.92).
narrative_ontology:measurement(dhar_be_t25, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 25, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0, 0.82).
narrative_ontology:measurement(dhar_su_t3, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 3, 0.84).
narrative_ontology:measurement(dhar_su_t8, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 8, 0.86).
narrative_ontology:measurement(dhar_su_t13, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 13, 0.87).
narrative_ontology:measurement(dhar_su_t18, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 18, 0.88).
narrative_ontology:measurement(dhar_su_t25, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 25, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__abolitionist_rejection, identity_coordination).
narrative_ontology:boltzmann_floor_override(dharmasastra_corpus__abolitionist_rejection, 0.05).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__reformist_contextual).

% DUAL FORMULATION NOTE:
% The Dharmasastra corpus constraint family decomposes into three structurally distinct readings: abolitionist_rejection (this file, ε=0.92, snare, zero textual authority, victim-centered), orthodox_literalist (ε≈0.15-0.30, rope or mountain, eternal revealed truth, hierarchy justified), reformist_contextual (ε≈0.45-0.65, tangled_rope, historical wisdom separable from time-bound prescriptions). Each reading instantiates a different constraint from the same kernel (Sanskrit texts). The readings differ fundamentally on whether textual authority is revealed (eternal) or contingent (historical), whether the hierarchy is the core or a layer, and whether alternatives exist. They coexist as live positions held by different Hindu communities. The abolitionist reading forecloses the orthodox reading by denying revealed authority entirely; it influences the reformist reading by creating pressure to abandon attempts at salvage and accept complete rejection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
