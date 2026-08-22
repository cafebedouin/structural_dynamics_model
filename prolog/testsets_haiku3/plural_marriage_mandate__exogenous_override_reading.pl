% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__exogenous_override_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: plural_marriage_mandate__exogenous_override_reading
 *   human_readable: Federal Coercion Against Plural Marriage (Exogenous Override Reading)
 *   domain: religious/political/institutional
 *
 * SUMMARY:
 *   Between 1862 and 1890, the federal government intensified suppression of
 *   plural marriage in the LDS church through criminal statutes, property
 *   seizure, and imprisonment of practitioners. The 1890 Manifesto is the
 *   church leadership's declaration that plural marriage is suspended by
 *   divine revelation. The exogenous override reading interprets this not as
 *   legitimate doctrinal reinterpretation, but as coerced abandonment of a
 *   foundational practice masked in revelation language. The constraint is
 *   the federal coercion forcing the Manifesto, not the Manifesto itself —
 *   the Manifesto is the extraction's face-saving outcome. Under this
 *   reading, the constraint operates as a snare: extracting the church's
 *   abandonment of plural marriage through suppression costs (property loss,
 *   imprisonment), while the church's own revelation authority is repurposed
 *   to legitimize the extraction as voluntary divine will. The claim/metric
 *   gap is deliberate: the constraint is CLAIMED as a snare (coercive
 *   extraction) while the authored metrics show suppression approaching
 *   ceiling (0.92), extraction accumulating over decades (0.15→0.89), and
 *   theater rising sharply after 1880 (when the Manifesto becomes imminent) —
 *   the engine measures how a suppression-driven outcome is renarrated as
 *   doctrinal revelation.
 *
 * KEY AGENTS:
 *   - Federal government: institutional agenda-setter, enforces territorial uniformity through anti-polygamy statutes, collects property, imprisons practitioners, holds coercive authority
 *   - Practicing polygamists: powerless victims, trapped between apostasy and criminalization, face imprisonment, property seizure, family separation
 *   - LDS church institutional: organized victim and captive beneficiary, loses property and standing but survives by capitulating, repurposes revelation authority to legitimize the capitulation
 *   - Federal law authority: competing institutional authority system that subordinates religious practice to civil law
 *   - Polygamist families (excluded): would testify to persecution and coercion but are not part of the institutional negotiation that produces the Manifesto
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, 0.89).
domain_priors:suppression_score(plural_marriage_mandate__exogenous_override_reading, 0.92).
domain_priors:theater_ratio(plural_marriage_mandate__exogenous_override_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__exogenous_override_reading, snare).
narrative_ontology:human_readable(plural_marriage_mandate__exogenous_override_reading, "Federal Coercion Against Plural Marriage (Exogenous Override Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__exogenous_override_reading, "religious/political/institutional").

domain_priors:requires_active_enforcement(plural_marriage_mandate__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__exogenous_override_reading, '08c9d129-222f-451e-9e83-3e175b0cba5c').
narrative_ontology:cs_kernel_codification('08c9d129-222f-451e-9e83-3e175b0cba5c', fixed_text).
narrative_ontology:cs_authority_grounding('08c9d129-222f-451e-9e83-3e175b0cba5c', extraction).
narrative_ontology:cs_interpretation_layer_present('08c9d129-222f-451e-9e83-3e175b0cba5c').
narrative_ontology:cs_reading_relation('08c9d129-222f-451e-9e83-3e175b0cba5c', plural_marriage_mandate__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('08c9d129-222f-451e-9e83-3e175b0cba5c', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('08c9d129-222f-451e-9e83-3e175b0cba5c', foundational, federal_coercion_invalidates_revelation_claim).
narrative_ontology:cs_axiom_status(federal_coercion_invalidates_revelation_claim, holdable).
narrative_ontology:cs_axiom_grounding('08c9d129-222f-451e-9e83-3e175b0cba5c', federal_coercion_invalidates_revelation_claim, deontological).
narrative_ontology:cs_axiom('08c9d129-222f-451e-9e83-3e175b0cba5c', foundational, doctrinal_shifts_driven_by_external_power_are_extraction_not_reinterpretation).
narrative_ontology:cs_axiom_status(doctrinal_shifts_driven_by_external_power_are_extraction_not_reinterpretation, holdable).
narrative_ontology:cs_axiom_grounding('08c9d129-222f-451e-9e83-3e175b0cba5c', doctrinal_shifts_driven_by_external_power_are_extraction_not_reinterpretation, empirically_contingent).
narrative_ontology:cs_reference_frame('08c9d129-222f-451e-9e83-3e175b0cba5c', divine_requirement_to_practice_plural_marriage).
narrative_ontology:cs_drift_state('08c9d129-222f-451e-9e83-3e175b0cba5c', post_manifesto_1890, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('08c9d129-222f-451e-9e83-3e175b0cba5c', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, lds_church_institutional).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, lds_church_institutional).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, monogamic_settlers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces territorial uniformity and subordination of religious practice to federal law through anti-polygamy statutes, property seizure, imprisonment of practitioners, and disfranchisement. Controls the coercive machinery (marshals, courts, territorial legislature). Justifies enforcement as establishing law-of-the-land supremacy and monogamic moral norms. Benefits directly from the suppression: achieves political control, collects confiscated property, establishes federal authority over the territory.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% Face criminal prosecution, property seizure, imprisonment, and family separation. Their exit options are: renounce the practice (experienced as coerced apostasy), relocate outside US territory (increasingly impossible), or maintain the practice in criminal hiding. They experience federal law as persecution, not legitimate governance. The constraint forces them to choose between theological conviction and legal safety; it permits no stable third option.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists, payer,
    powerless, biographical, trapped, regional).

% Bears institutional costs: confiscated temples and properties, loss of political standing, persecution of leadership and members. The church faces existential pressure: federal law permits it to survive only if it abandons plural marriage. The 1890 Manifesto is the church's response — nominally a divine revelation, read under this constraint as capitulation to superior coercive power. The church's identity is fused with revelation authority; it cannot simply abandon the authority structure without ceasing to be the LDS church. Thus its exit options are identity-locked: it must survive through the revelation mechanism, not by adopting a different legitimacy source.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, lds_church_institutional, payer,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__exogenous_override_reading, lds_church_institutional, beneficiary).

% Gain access to the Utah territory on terms compatible with their own monogamic family structures and moral norms. Benefit from federal enforcement of territorial uniformity without bearing suppression costs directly (the federal government absorbs those costs). Their interests align with federal authority, and the constraint protects those interests.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, monogamic_settlers, beneficiary,
    organized, biographical, mobile, national).

% Would testify to the lived experience of federal persecution: forced family separations, hidden household structures, children born in legal jeopardy, women's economic vulnerability under prosecution. Their voices are completely absent from the institutional negotiations that produce the Manifesto. They have no seat in the decision-making process, yet bear the highest personal cost.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, lds_polygamist_families, excluded,
    powerless, biographical, trapped, regional).

% The abstract institutional claim that federal civil law supersedes religious practice in US territory. Enforced through the power of the federal government. After the Manifesto, federal law authority is nominally no longer contested because the church's own doctrine aligns with federal statutes. The constraint succeeds when this authority becomes invisible — when religious doctrine appears to spontaneously align with federal law rather than being coerced into alignment.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, federal_law_authority, agenda_setter,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(plural_marriage_mandate__exogenous_override_reading, federal_law_authority).

% The abstract institutional claim that God reveals doctrine through the LDS church's prophet. This authority structure survives the Manifesto by being repurposed: the same revelation machinery that authorized plural marriage now authorizes its suspension. The doctrine shifts, but the authority mechanism is preserved. Under this reading, the church's leadership has strong incentive to preserve revelation authority even at the cost of abandoning specific doctrines, because the authority itself is the church's core institutional claim.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, revelation_authority, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(plural_marriage_mandate__exogenous_override_reading, revelation_authority).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is no coordination function. The constraint serves federal territorial control and authority subordination, not a collective-action problem that plural marriage inhibits.
% TRANSFER_FUNCTION: Transfers property (confiscated estates and temples), freedom (imprisonment of practitioners), doctrinal authority (from church revelation to federal law supremacy), and family structures (criminalization forces separation or hiding). The 1890 Manifesto transfers the church's own doctrinal apparatus to enforcement: the church becomes the mechanism by which federal policy is executed and legitimized.
% ABSENT_VOICES: Practicing polygamists and their families are excluded. They would testify that the constraint is coercive extraction, not legitimate governance or doctrinal development. Women in plural marriages would speak to economic and legal vulnerability. Underground practitioners and those who fled to Canada/Mexico would testify to the lived pressure and impossibility of exit. The institutional negotiation that produces the Manifesto involves federal officials and church leadership; it does not include those who bear the suppression costs.
% DISAPPEARANCE_RATIONALE: If the federal suppression machinery and the 1890 Manifesto constraint vanished, the LDS church would resume authorizing plural marriage (as it has in breakaway communities); practicing polygamists would emerge from hiding or return from exile; confiscated property would be reclaimed or claimed; and the church's institutional authority would be radically reorganized. The constraint's removal would trigger massive religious, family, and institutional restructuring.
% FOUNDING_PROBLEM: The founding problem is federal government's need to establish territorial uniformity and subordinate religious practice to federal law authority in the Utah territory. This is framed as an issue of 'the Mormon problem' or 'the polygamy problem,' but the underlying federal interest is political control.
% FOUNDING_PROBLEM_CORROBORATION: Congressional records, federal legislative debate about Utah statehood, statements by territorial governors and federal marshals explicitly identify the problem as the LDS church's defiance of federal authority and its practice of plural marriage as a territorial governance challenge. The federal intent is clear and documented outside the benefiting parties (the federal government itself). The church does not frame the founding problem this way — it attributes the Manifesto to divine revelation, not to federal pressure. The exogenous reading attends to federal intent, not the church's framing.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(plural_marriage_mandate__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__exogenous_override_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extreme (0.89 at interval end) because the constraint extracts abandonment of a foundational religious practice under threat of criminalization and property loss — the extraction is the core structure. The measurements show accumulation: 1862–1870 (early anti-polygamy statutes, low extraction), 1870–1880 (Morrill Act strengthened, property seizure intensified, extraction rises sharply), 1880–1890 (Utah Territorial Commission, married polygamists are disfranchised, Manifesto imminent, extraction peaks), 1890–1910 (Manifesto in place, suppression stabilizes at ceiling because the church now self-enforces). Suppression is high (0.92) because the constraint persists through criminal law and active enforcement by federal marshals; it is not a matter of participant preference but of coercive power. Theater (0.78) is high throughout post-1880 because the church's revelation narrative increasingly dominates after the Manifesto — the suppression is no longer visibly federal force but appears as the church's own prophetic declaration. This rise in theater after 1890 is diagnostic of snare operation: the extraction persists, but the coercive face is hidden behind the institution's own doctrinal authority. Accessibility collapse (0.88) is high because polygamists' alternatives are severely restricted: they cannot practice within the church (post-Manifesto), cannot practice openly in US territory (federal law), and increasingly cannot access community (church enforcement post-Manifesto). Resistance (0.71) is substantial because practicing polygamists and families continued plural marriage underground, migration to Canada and Mexico occurred, and resistance petitions were filed within the church — the constraint required continuous enforcement to maintain.
 *
 * PERSPECTIVAL GAP:
 *   The federal agenda-setter seat (federal government) and the victim seats (practicing polygamists, LDS institutional leadership) should compute dramatically differently. From the federal seat, the constraint achieves territorial uniformity and law-of-the-land supremacy (a governance goal). From the victim seats, the same structure is coercive extraction: loss of theology, loss of community, loss of family structures, loss of property — enforced under criminal statute. The church's institutional position is peculiar: as payer (suffering suppression), it has strong incentive to name the constraint as coercive; but as the agent that authors the Manifesto (and thus becomes complicit in its own enforcement post-1890), it shifts toward the beneficiary end by maintaining institutional survival through capitulation. The engine computes this structural divergence from power/exit/beneficiary declarations — the church is both payer (powerless against federal force) and captive beneficiary (allowed to survive only if complicit). The exogenous override reading asserts that this apparent doctrinal choice is actually constrained behavior — the church has no real exit, only the choice to capitulate in its own voice or be suppressed by federal force.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government is the structural beneficiary: it collects territorial control and political supremacy, bears no suppression cost itself, and holds the coercive machinery (d near beneficiary end, ~0.05). Practicing polygamists are the clear targets: they bear criminalization, property loss, family disruption, and the cost of exile or hiding (d near target end, ~0.95). The LDS church institutional is the most complex: it is trapped between victimhood (suffering property loss, institutional humiliation, membership persecution) and forced complicity (after the Manifesto, it becomes the federal government's enforcement agent, suppressing its own practitioners). As a victim, it would have d near 0.85; as a forced beneficiary of its own survival (contingent on suppression), it carries dual directionality. The church's identity-locked exit (it cannot simply become another denomination; the institutional identity fuses with the revelation-claiming structure) drives the high measurement on exit constraints. Monogamic settlers benefit from federal enforcement but do not bear its cost; they sit near the beneficiary end. The secondary-role assignment on the church (both payer and beneficiary) reflects this structural bind.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classic mandatrophy features: the founding mandate is federal territorial uniformity and religious subordination (stated in congressional records, alive and energetically pursued 1862–1890). The founding function (ensuring federal law supremacy in the territory) remains live post-Manifesto, but the mandate has outlived its agency — after 1890, the church becomes the enforcement agent for federal policy, and federal effort decreases because suppression cost is externalized to the church. The theater ratio rising from 0.05 to 0.78 is diagnostic: early suppression is visibly federal (marshals, seizures, courts); late suppression is the church's own prophetic voice. The constraint persists as snare (high extraction, high suppression), but the coercive framing has shifted — the extraction now appears as divine will. The Manifesto is the key mandatrophy artifact: it is a document authored under coercion by the victim (the church), reusing the victim's own authority (revelation) to legitimize compliance. This is textbook snare evolution: the constraint persists by becoming self-enforcing, no longer requiring visible federal force. The classification prevents mislabeling this as rope (genuine coordination) or tangled rope (extractive but retaining coordination function): there is no coordination function here, only coercion. The suppression cost and extraction are inseparable — the coercion IS the mechanism by which the extraction is executed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_capitulation,
    'Is the 1890 Manifesto a genuine revelation (as the endogenous reinterpretation reading claims) or a capitulation to federal pressure dressed in revelation language (as the exogenous override reading claims)?',
    'Textual and historical analysis: examination of contemporary church records, prophet''s private writings, timing of doctrine shifts relative to federal pressure, and comparison with other doctrinal reversals in church history. If shifts are consistently reactive to political pressure, exogenous framing is supported; if they show independent theological development, endogenous framing gains ground.',
    'If exogenous (coerced), the constraint is snare: extraction masquerading as choice. If endogenous (genuine reinterpretation), the constraint is rope or tangled rope: coordination-driven doctrine with asymmetric extraction. The classification hinges on this fundamental ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_vs_capitulation, empirical, 'Whether the Manifesto represents genuine theological development or federal coercion.').

omega_variable(
    suppression_internalization,
    'To what extent is suppression of plural marriage structural (external coercion by federal force) versus internalized (the church''s own members come to believe the practice is wrong)?',
    'Historical witness accounts from practicing polygamists and their families; evidence of underground practice post-Manifesto; migration patterns; testimony about the subjective experience of coercion versus conviction. Post-exit trajectories: do people who leave the church continue plural marriage (suggesting coercion was external), or do they abandon it (suggesting internalization)?',
    'If suppression is primarily structural, the constraint''s effective suppression is the authored 0.92 (federal coercion). If substantially internalized, suppression persists even after exit from the institutional context, raising the effective long-term extraction cost. This affects the snare classification: a purely structural snare can theoretically be escaped by exit; an internalized one travels with the agent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'The mechanism of suppression: structural coercion or internalized conviction.').

omega_variable(
    authority_preservation_motive,
    'Did the church''s leadership choose the Manifesto primarily to preserve the institutional church and revelation authority, or to spare practitioners further persecution, or some weighted combination?',
    'Analysis of contemporary church leadership communication: internal deliberations, statements about why the Manifesto was necessary, post-Manifesto institutional strategy. Attention to whether institutional survival or member welfare was foregrounded. Comparison with other institutional choices during the suppression period.',
    'If institutional survival was primary, the church''s strategic position is captive beneficiary: it survives by becoming complicit. If member welfare was primary, the church''s position is more clearly victim. The directionality and role assessment shift accordingly. This also feeds the institutional_pragmatism reading''s characterization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_preservation_motive, conceptual, 'The church''s primary motive in authorizing the Manifesto.').

omega_variable(
    kernel_kernel_contest_asymmetry,
    'Why does the exogenous override reading treat federal coercion as determinative while the endogenous reinterpretation reading treats it as irrelevant to the theological truth of the Manifesto?',
    'Explicit statement: the exogenous reading prioritizes causal mechanism (federal pressure caused the doctrinal shift), while the endogenous reading prioritizes doctrinal authority (God revealed the shift, making the cause irrelevant to its legitimacy). These are different questions about different referents — one is empirical-historical, the other is theological-normative. The asymmetry reflects a disagreement about whether historical causation undermines theological authority.',
    'This omega documents the fundamental reading divergence: not an empirical dispute resolvable by more data, but a dispute about what kind of question matters. A party that accepts God reveals through coercion-responsive prophets could hold both exogenous mechanism AND endogenous authority. A party that treats exogenous coercion as invalidating revelation authority cannot. The constraint classification depends on which reading framework is adopted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_kernel_contest_asymmetry, conceptual, 'The kernel contest hinges on whether federal causation negates or is orthogonal to theological authority.').

omega_variable(
    coerced_doctrine_legitimacy,
    'Can a doctrinal claim (that God commanded suspension of plural marriage) be legitimate if its production is demonstrably coerced by secular power, even if the doctrine''s content is internally coherent to the tradition?',
    'This is a normative question about religious authority, not an empirical one. It depends on philosophical premises about doctrine, coercion, and authority. A theologically integrated answer would require assent to premises about how God acts through pressure, what counts as legitimate revelation, and whether coercive context invalidates a doctrinal claim. Different religious frameworks (the endogenous reinterpretation reads coercion as compatible with revelation; the exogenous override reads it as invalidating) produce different answers.',
    'If coercion invalidates doctrinal legitimacy, the Manifesto cannot be treated as legitimate reinterpretation, and the exogenous reading''s snare classification holds. If coercion is orthogonal to doctrinal legitimacy (God acts through whatever circumstances, including pressure), the endogenous reading stands, and classification shifts toward rope or tangled rope. The constraint type hinges on resolving this normative question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coerced_doctrine_legitimacy, preference, 'Normative question: does coerced production of doctrine undermine its theological legitimacy?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__exogenous_override_reading, 1862, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1862, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1862, 0.05).
narrative_ontology:measurement_basis(plur_tr_t1862, observed).
narrative_ontology:measurement(plur_tr_t1870, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1870, 0.12).
narrative_ontology:measurement_basis(plur_tr_t1870, observed).
narrative_ontology:measurement(plur_tr_t1880, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1880, 0.35).
narrative_ontology:measurement_basis(plur_tr_t1880, observed).
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1890, 0.75).
narrative_ontology:measurement_basis(plur_tr_t1890, observed).
narrative_ontology:measurement(plur_tr_t1900, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1900, 0.78).
narrative_ontology:measurement_basis(plur_tr_t1900, observed).
narrative_ontology:measurement(plur_tr_t1910, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1910, 0.78).
narrative_ontology:measurement_basis(plur_tr_t1910, observed).

% Extraction over time
narrative_ontology:measurement(plur_be_t1862, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1862, 0.15).
narrative_ontology:measurement_basis(plur_be_t1862, observed).
narrative_ontology:measurement(plur_be_t1870, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1870, 0.42).
narrative_ontology:measurement_basis(plur_be_t1870, observed).
narrative_ontology:measurement(plur_be_t1880, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1880, 0.75).
narrative_ontology:measurement_basis(plur_be_t1880, observed).
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1890, 0.86).
narrative_ontology:measurement_basis(plur_be_t1890, observed).
narrative_ontology:measurement(plur_be_t1900, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1900, 0.89).
narrative_ontology:measurement_basis(plur_be_t1900, observed).
narrative_ontology:measurement(plur_be_t1910, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1910, 0.89).
narrative_ontology:measurement_basis(plur_be_t1910, observed).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1862, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1862, 0.25).
narrative_ontology:measurement_basis(plur_su_t1862, observed).
narrative_ontology:measurement(plur_su_t1870, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1870, 0.58).
narrative_ontology:measurement_basis(plur_su_t1870, observed).
narrative_ontology:measurement(plur_su_t1880, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1880, 0.82).
narrative_ontology:measurement_basis(plur_su_t1880, observed).
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1890, 0.9).
narrative_ontology:measurement_basis(plur_su_t1890, observed).
narrative_ontology:measurement(plur_su_t1900, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1900, 0.92).
narrative_ontology:measurement_basis(plur_su_t1900, observed).
narrative_ontology:measurement(plur_su_t1910, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1910, 0.92).
narrative_ontology:measurement_basis(plur_su_t1910, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(plural_marriage_mandate__exogenous_override_reading, 0.05).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate__institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% The plural_marriage_mandate kernel admits three distinct constraint readings: (1) exogenous_override_reading (federal coercion, snare), (2) endogenous_reinterpretation_reading (theological development, rope/tangled rope), (3) institutional_pragmatism_reading (capitulation framed as doctrine). Each reading instantiates a different constraint with different ε, different beneficiary/victim structure, different type classification. They are not measurements of the same constraint but readings of a contested kernel that produce structurally distinct constraints. The exogenous reading treats the 1890 Manifesto as an outcome of federal suppression; the endogenous reading treats it as a genuine theological development; the pragmatism reading treats it as strategic reframing of capitulation. All three are live positions held by different parties in the historical dispute. This story instantiates the exogenous reading only; the siblings are separate constraint stories linked by the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
