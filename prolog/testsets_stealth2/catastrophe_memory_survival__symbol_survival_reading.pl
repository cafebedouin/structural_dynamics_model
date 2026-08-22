% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__symbol_survival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__symbol_survival_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__symbol_survival_reading
 *   human_readable: Symbol-Survival Reading: Ritual Form Preservation as Collective Survival
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   In the post-catastrophe reconstruction of Jewish communal life, the
 *   operative standard examined here holds that collective survival consists
 *   in continuity of ritual practice itself: identity and boundary-norms are
 *   carried by symbolic experience, forms are to be preserved invariantly,
 *   and a household that abandons practice is counted among the community's
 *   dead. The standard is administered by rabbinic authority through
 *   education systems, adjudication, and communal sanction. This file
 *   instantiates ONE reading of the catastrophe_memory_survival kernel — the
 *   symbol-survival reading — as a clean, epsilon-invariant constraint; the
 *   competence-transmission and hybrid-encoding readings are separate stories
 *   with their own epsilon values, beneficiary structures, and
 *   classifications, linked through network.affects_constraints. Within the
 *   constraint family, this reading carries the highest extraction profile
 *   for form preservation (epsilon 0.72): the competence-transmission reading
 *   locates ritual's survival value in embedded practical knowledge
 *   (extraction attaches mainly to knowledge gatekeeping), and the hybrid
 *   reading distributes extraction across both registers, while this reading
 *   concentrates it on form invariance and the 'lost'-member classification.
 *   Claim and metrics are independent authored facts: the tangled_rope claim
 *   states what I believe is structurally true — a genuine coordination
 *   function with asymmetric extraction riding on it — while the metric
 *   values describe the arrangement's actual operation as this reading
 *   assesses it.
 *
 * KEY AGENTS:
 *   - rabbinic_interpretive_authority: agenda-setter and primary beneficiary (institutional / identity_locked) — defines correct practice, collects interpretive deference, administers transmitting institutions
 *   - observant_practicing_community: beneficiary (organized / identity_locked) — performs and funds the preserved forms, receives identity and continuity
 *   - secularized_jews: primary target (moderate / constrained) — bear the 'lost' classification, transmission pressure on their children, relational exit costs
 *   - secular_zionist_nation_builders: excluded rival (powerful / mobile) — built competing survival vehicles outside the frame
 *   - jewish_studies_historians: analytical observer (analytical / analytical) — sees the full comparative record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, 0.72).
domain_priors:suppression_score(catastrophe_memory_survival__symbol_survival_reading, 0.71).
domain_priors:theater_ratio(catastrophe_memory_survival__symbol_survival_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__symbol_survival_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__symbol_survival_reading, "Symbol-Survival Reading: Ritual Form Preservation as Collective Survival").
narrative_ontology:topic_domain(catastrophe_memory_survival__symbol_survival_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__symbol_survival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__symbol_survival_reading, '8891d153-092f-42b3-a822-ef54c59e5803').
narrative_ontology:cs_kernel_codification('8891d153-092f-42b3-a822-ef54c59e5803', distributed).
narrative_ontology:cs_authority_grounding('8891d153-092f-42b3-a822-ef54c59e5803', lineage).
narrative_ontology:cs_interpretation_layer_present('8891d153-092f-42b3-a822-ef54c59e5803').
narrative_ontology:cs_reading_relation('8891d153-092f-42b3-a822-ef54c59e5803', catastrophe_memory_survival__competence_transmission_reading, forecloses).
narrative_ontology:cs_reading_relation('8891d153-092f-42b3-a822-ef54c59e5803', catastrophe_memory_survival__hybrid_encoding_reading, forecloses).
narrative_ontology:cs_axiom('8891d153-092f-42b3-a822-ef54c59e5803', foundational, practice_continuity_constitutes_survival).
narrative_ontology:cs_axiom_status(practice_continuity_constitutes_survival, holdable).
narrative_ontology:cs_axiom_grounding('8891d153-092f-42b3-a822-ef54c59e5803', practice_continuity_constitutes_survival, conventional).
narrative_ontology:cs_axiom('8891d153-092f-42b3-a822-ef54c59e5803', secondary, boundary_preservation_requires_form_invariance).
narrative_ontology:cs_axiom_status(boundary_preservation_requires_form_invariance, holdable).
narrative_ontology:cs_axiom_grounding('8891d153-092f-42b3-a822-ef54c59e5803', boundary_preservation_requires_form_invariance, empirically_contingent).
narrative_ontology:cs_reference_frame('8891d153-092f-42b3-a822-ef54c59e5803', practice_continuity_identity_frame).
narrative_ontology:cs_drift_state('8891d153-092f-42b3-a822-ef54c59e5803', contemporary_secular_diaspora, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8891d153-092f-42b3-a822-ef54c59e5803', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, rabbinic_interpretive_authority).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, observant_practicing_community).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, secularized_jews).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__symbol_survival_reading, continuity_of_practice_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__symbol_survival_reading, form_invariance_transmission_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines which practices are obligatory, trains and credentials the teachers who transmit them, adjudicates disputes over correct form, and administers the institutions (day schools, religious courts, dietary certification) through which continuity is maintained. Interpretive deference and institutional resourcing flow to this seat, and its standing rests on the premise that preserved form, not adaptation, is what the community survived by. Leaving the role would mean forfeiting the very authority the role constitutes.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, rabbinic_interpretive_authority, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__symbol_survival_reading, rabbinic_interpretive_authority, beneficiary).

% Performs the preserved forms daily and seasonally, funds the transmitting institutions, and receives in return a dense identity, a calendrical rhythm, and membership in an unbroken chain reaching back through the catastrophe. Observance burdens and marriage-pool constraints fall on this seat as well, but within the community's own accounting these read as the price of belonging rather than an imposition. Exit is materially possible yet experienced as amputation of family, meaning, and self-description.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, observant_practicing_community, beneficiary,
    organized, generational, identity_locked, global).

% Have reduced or abandoned ritual practice while retaining ethnic or cultural affiliation. Under the continuity-of-practice standard their households are recorded as transmission failures — the community's dead — regardless of their own flourishing. They absorb the sanction carried by that classification: family rupture, communal grief aimed at them as loss, and children targeted by re-engagement campaigns. Material exit is available and frequently taken; the lasting costs are relational and reputational, and they land hardest on the second generation, who inherit the deficit without having chosen it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, secularized_jews, payer,
    moderate, biographical, constrained, global).

% Built rival vehicles of persistence — revived Hebrew, national institutions, secular culture — and argued openly that peoplehood endures through language and polity rather than ritual form. They stand outside the frame in which continuity-of-practice adjudicates survival, so the constraint does not bind them; their flourishing is the standing counterexample the frame must explain away, and they are not present when the continuity standard is applied to count who was lost.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, secular_zionist_nation_builders, excluded,
    powerful, generational, mobile, national).

% Study which communities persisted through catastrophe and by what mechanisms, comparing practice-continuous, practice-transformed, and secularly-identified populations across cases. They neither collect from the arrangement nor pay into it, and from their seat both the rabbinic account and its rivals are visible as partial descriptions of the same record.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, jewish_studies_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__symbol_survival_reading, rabbinic_interpretive_authority).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__symbol_survival_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Shared preserved practice synchronizes a territorially dispersed population — one calendar, one lifecycle script, one dietary boundary, one liturgy — so that scattered, persecuted communities remain mutually recognizable and continuously re-derivable as a single people across generations without territory or sovereignty.
% TRANSFER_FUNCTION: Moves interpretive deference and institutional resourcing upward to the rabbinic class, and moves the cost of boundary-maintenance downward onto marginal members: those least able or willing to perform the preserved forms, whose departure is reclassified from legitimate choice to communal death.
% ABSENT_VOICES: Secularized Jews themselves and the secular Zionist nation-builders are largely outside the frame in which 'survival equals practice continuity' is adjudicated. Present, they would argue that language, nationhood, and chosen affiliation are equally real vehicles of persistence, and that a standard which counts them as lost is authored by the authority that benefits from the counting.
% DISAPPEARANCE_RATIONALE: If the form-preservation standard vanished overnight, identity would reorganize around the rival vehicles already visible at the margins — language revival, national citizenship, voluntary cultural affiliation — the rabbinic interpretive monopoly would dissolve into one voice among several, and the population currently scored as 'lost' would be re-described as members of differently-shaped continuations rather than as the community's dead.
% FOUNDING_PROBLEM: After repeated catastrophic rupture — destruction of the Temples, expulsions, massacres, and finally the Holocaust — a territorially dispersed people faced the problem of persisting as a distinct, continuous community across generations without sovereignty, territory, or coercive power of its own.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem itself is corroborated from outside the benefiting parties: the recurrence of catastrophic rupture is independently documented historiography. Its status splits along the same line as the kernel contest — studies of crypto-Jewish persistence and of day-school transmission cohorts support practice-continuity effects, while studies of Yiddish secular culture, Bundist institutions, and Israeli nation-building show identity persisting without form continuity. No source outside the rabbinic set attests that form-invariance specifically remains the live mechanism; the parties dispute it.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__symbol_survival_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__symbol_survival_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__symbol_survival_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_survival__symbol_survival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__symbol_survival_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the transfer is real and asymmetric: interpretive deference and institutional resourcing flow to the authority that defines the forms, while the cost of the boundary — the 'lost' classification, family rupture, re-engagement pressure on children — lands on those least positioned to contest the standard. Suppression (0.71) is a raw structural property, unscaled by power or scope: it combines structural enforcement (day-school systems, communal sanction, marriage-pool pressure) with internalized force (obligation felt toward the murdered dead, identity fused with practice), and the omega variable records that the split between these mechanisms is unresolved. Theater ratio (0.36) has risen steadily across the interval: a growing share of maintained practice is performed heritage — commemorative attendance, lifecycle-event identification, museum-and-ancestry Judaism — rather than lived normative commitment, though the observant core remains functionally engaged. Accessibility collapse (0.58) is partial: inside the frame, understanding the standard collapses alternatives almost completely, since abandoning practice is defined as consenting to collective death; but modern exit routes (secular identity, intermarriage, disaffiliation) remain materially open, so collapse is far short of natural-law completeness. Resistance (0.62) is sustained and organized: Reform and Reconstructionist practice-change, secular Zionism, Bundism, assimilation, and mass disaffiliation are all active refusals of the form-invariance demand. The three measurement series share one time grid (eight points, 1945–2020) so every metric is authored at every examined point; all three rise, modeling enforcement infrastructure maturing and hardening as exit options multiplied — the suppressive apparatus had to grow to hold a boundary that voluntary affiliation alone no longer held.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the divergence is the finding. From the rabbinic seat the arrangement is the community's life-support system: the same structure that extracts from the margin is, from the center, what made persistence possible at all, and the authority's identity-lock is institutional — the role has become the function, so questioning form-invariance reads as self-annihilation. From the observant seat the arrangement is mostly subsidy: identity, rhythm, and meaning received at a price experienced as belonging. From the secularized seat the same structure operates as a boundary machine that writes them out of the survival narrative and bills their children for re-entry. The historian seat sees all three as partial. Identity-lock binds the first two seats through professional and relational fusion respectively; if the frame broke — if survival were redefined as something practice continuity merely serves rather than is — the rabbinic seat's lock would break first, since its authority is constituted by the frame itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. Rabbinic interpretive authority sits nearest the beneficiary pole: it sets the rules, collects the deference, and bears little of the boundary's cost. The observant practicing community derives low directionality as a declared beneficiary, correctly so — it nets identity and continuity against real but self-priced observance costs. Secularized Jews derive high directionality as the declared victim group, amplified by their constrained exit: they can leave, but the classification follows the household, and the second generation inherits the deficit without having chosen it, which places them nearer the full-target end than their formal mobility suggests. The excluded nation-builders are not coordinated by this constraint at all — their exclusion is the enforcement object, not a seat inside it — and the observer seat takes no directional position.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Reading the arrangement as pure extraction (snare) would erase the genuine coordination function: synchronized practice across a dispersed, repeatedly-persecuted population was load-bearing during reconstruction, and communities that lost it did face measurably harder continuity problems. Reading it as pure coordination (rope) would erase the asymmetric transfer: an interpretive monopoly, a 'lost'-member sanction, and enforcement machinery that grew as exit options widened are not overhead of coordination but extraction layered on it. On mandatrophy: the founding problem — persistence under recurrent catastrophe — is not dead, since the conditions recur, so the arrangement cannot be dismissed as vestigial; but the specific form-invariance demand shows classic drift symptoms (rising theater ratio, enforcement intensification against falling voluntary compliance), which is why the founding-problem status is authored as contested rather than live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the symbol_survival_reading of the catastrophe_memory_survival kernel. What would the sibling readings (competence_transmission_reading, hybrid_encoding_reading) change structurally, and where exactly is the disagreement located?',
    'Comparative analysis of which post-catastrophe communities persisted under practice-transformation versus practice-continuity, controlling for coercion, geography, and host-society conditions; the disagreement is located in what carries the survival-relevant content — symbolic form itself, embedded practical knowledge, or both.',
    'If the competence reading prevails, the victim set shifts from those losing symbolic participation to those losing practical knowledge, and rabbinic interpretive control loses its survival warrant; if the hybrid reading prevails, extraction redistributes across both registers and this reading''s concentrated epsilon decomposes into two smaller components.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: kernel membership, reading identity, sibling deltas, and disagreement location.').

omega_variable(
    survival_referent_definition,
    'Does ''survival'' denote demographic persistence, cultural continuity, or halakhic continuity — and who has standing to define it?',
    'Conceptual analysis combined with survey of how persisting communities themselves describe what was preserved; the reading''s own definition (practice continuity) is a stipulation, not an observation.',
    'Under demographic or cultural definitions, secularized Jews are survivors rather than victims, the declared victim set collapses, and the measured extraction drops sharply; under the halakhic definition the current victim set and high epsilon stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_referent_definition, conceptual, 'Whether the constraint''s victim structure is real or an artifact of the reading''s own success criterion.').

omega_variable(
    counterfactual_vehicle_sufficiency,
    'Could the post-catastrophe community have persisted demographically through non-ritual vehicles — language revival, nationhood, secular institutions — at comparable rates?',
    'Comparative diaspora studies: Armenian and other catastrophe-surviving diasporas, crypto-Jewish descendant communities, Yiddishist secular persistence, and Israeli nation-building outcomes.',
    'If alternative vehicles suffice, the constraint''s coordination claim shrinks toward cover story and the arrangement trends snare-flavored; if they do not, a substantial share of the measured extraction is the price of genuine survival machinery and the tangled_rope reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_vehicle_sufficiency, empirical, 'Whether the coordination function attributed to form preservation is dispensable.').

omega_variable(
    suppression_structural_internalized_split,
    'Is the measured suppression structural (school systems, communal sanction, marriage-market pressure) or internalized (obligation toward the murdered dead, identity fused with practice)?',
    'Post-exit trajectory study of disaffiliated members: if the felt obligation and boundary-guilt persist after leaving the enforcing community, the internalized component dominates; if they decay with distance, the structural component dominates.',
    'If largely internalized, effective suppression exceeds the structural measure and travels with the target past exit — enforcement-capacity measurements understate the constraint''s hold, and the victim set extends to the second generation in a stronger sense than the current authoring captures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_internalized_split, empirical, 'Structural versus internalized composition of the constraint''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__symbol_survival_reading, 1945, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t1945, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 1945, 0.14).
narrative_ontology:measurement(cata_tr_t1955, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 1955, 0.16).
narrative_ontology:measurement(cata_tr_t1965, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 1965, 0.19).
narrative_ontology:measurement(cata_tr_t1975, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 1975, 0.22).
narrative_ontology:measurement(cata_tr_t1985, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(cata_tr_t1995, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 1995, 0.29).
narrative_ontology:measurement(cata_tr_t2005, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 2005, 0.33).
narrative_ontology:measurement(cata_tr_t2020, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 2020, 0.36).

% Extraction over time
narrative_ontology:measurement(cata_be_t1945, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 1945, 0.55).
narrative_ontology:measurement(cata_be_t1955, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 1955, 0.58).
narrative_ontology:measurement(cata_be_t1965, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 1965, 0.61).
narrative_ontology:measurement(cata_be_t1975, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 1975, 0.63).
narrative_ontology:measurement(cata_be_t1985, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 1985, 0.66).
narrative_ontology:measurement(cata_be_t1995, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 1995, 0.68).
narrative_ontology:measurement(cata_be_t2005, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 2005, 0.7).
narrative_ontology:measurement(cata_be_t2020, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 2020, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t1945, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 1945, 0.52).
narrative_ontology:measurement(cata_su_t1955, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 1955, 0.57).
narrative_ontology:measurement(cata_su_t1965, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 1965, 0.62).
narrative_ontology:measurement(cata_su_t1975, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 1975, 0.65).
narrative_ontology:measurement(cata_su_t1985, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 1985, 0.67).
narrative_ontology:measurement(cata_su_t1995, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 1995, 0.69).
narrative_ontology:measurement(cata_su_t2005, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(cata_su_t2020, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 2020, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__symbol_survival_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the catastrophe_memory_survival kernel per the epsilon-invariance principle: the colloquial label 'ritual as survival mechanism' conflates three structurally distinct claims. This story authors the symbol-survival reading (high epsilon, 0.72, concentrated on form invariance and the lost-member classification; beneficiary: rabbinic interpretive authority; victims: secularized Jews). The competence-transmission reading authors ritual as practical-knowledge encoding with a different epsilon profile and different extraction surface (knowledge gatekeeping rather than form policing). The hybrid-encoding reading authors the dual-register claim with extraction distributed across both. Each file carries its own stable epsilon, beneficiaries, and victims; the edges here record that the readings compete over the same empirical record and that this reading's institutional dominance shapes the resource environment in which the siblings are argued.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
