% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: plural_marriage_mandate__endogenous_reinterpretation_reading
 *   human_readable: Prophetic Reinterpretation of Plural Marriage Mandate (Endogenous Reading)
 *   domain: religious_institutional_history/commitment_systems
 *
 * SUMMARY:
 *   The 1890 Manifesto is the Latter-day Saint Church's official declaration
 *   suspending the practice of plural marriage, issued by Church President
 *   Wilford Woodruff under enormous federal legal pressure. From the
 *   endogenous reinterpretation reading, the Manifesto represents legitimate
 *   prophetic reinterpretation: God revealed to President Woodruff that
 *   plural marriage, while eternally doctrinal, is subject to temporal
 *   suspension when institutional survival requires it. The constraint
 *   redistributes institutional belonging and sacramental standing away from
 *   fundamentalist practitioners (who refused the suspension) and toward the
 *   mainstream membership accepting the new prophetic direction. This reading
 *   frames the change as doctrinally coherent (the original revelation D&C
 *   132 remains binding; only its practice is suspended) and institutionally
 *   stabilizing (the church survives federal legal assault while preserving
 *   its salvific mission). The reading's core claim is that the Manifesto
 *   resolves a theological question (the temporality of plural marriage)
 *   through legitimate prophetic channels, not through coerced institutional
 *   pragmatism. The alternative readings—exogenous override and institutional
 *   pragmatism—contest this framing by emphasizing federal coercion and
 *   strategic adaptation rather than endogenous revelation. This constraint
 *   story instantiates ONLY the endogenous reinterpretation reading, author
 *   the metrics and stakes it produces, and routes committer structure (the
 *   contested kernel and sibling readings) to omega variables.
 *
 * KEY AGENTS:
 *   - lds_institutional_leadership: Agenda setter (institutional power). Declares and enforces the Manifesto as legitimate prophetic direction; controls the narrative framing and benefits from institutional continuity.
 *   - mainstream_lds_membership: Beneficiary (moderate power, constrained exit). Accept the Manifesto as divinely sanctioned; gain legal safety and institutional coherence.
 *   - fundamentalist_practitioners: Payer (powerless, identity-locked). Refuse the Manifesto; experience excommunication and institutional severing; trapped between marriage and belonging.
 *   - federal_government: Excluded (institutional power). The external coercive pressure; absent from the Manifesto's legitimacy narrative.
 *   - fundamentalist_interpreters: Excluded (powerless, identity-locked). Maintain the original reading; structurally delegitimized by the church's institutional framing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.38).
domain_priors:suppression_score(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.62).
domain_priors:theater_ratio(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(plural_marriage_mandate__endogenous_reinterpretation_reading, "Prophetic Reinterpretation of Plural Marriage Mandate (Endogenous Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__endogenous_reinterpretation_reading, "religious_institutional_history/commitment_systems").

domain_priors:requires_active_enforcement(plural_marriage_mandate__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__endogenous_reinterpretation_reading, 'e5177463-a128-4fa4-a389-507fd498f68b').
narrative_ontology:cs_kernel_codification('e5177463-a128-4fa4-a389-507fd498f68b', fixed_text).
narrative_ontology:cs_authority_grounding('e5177463-a128-4fa4-a389-507fd498f68b', lineage).
narrative_ontology:cs_interpretation_layer_present('e5177463-a128-4fa4-a389-507fd498f68b').
narrative_ontology:cs_reading_relation('e5177463-a128-4fa4-a389-507fd498f68b', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5177463-a128-4fa4-a389-507fd498f68b', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('e5177463-a128-4fa4-a389-507fd498f68b', foundational, prophetic_reinterpretation_is_doctrinally_coherent).
narrative_ontology:cs_axiom_status(prophetic_reinterpretation_is_doctrinally_coherent, holdable).
narrative_ontology:cs_axiom_grounding('e5177463-a128-4fa4-a389-507fd498f68b', prophetic_reinterpretation_is_doctrinally_coherent, deontological).
narrative_ontology:cs_axiom('e5177463-a128-4fa4-a389-507fd498f68b', foundational, temporal_suspension_preserves_eternal_mandate).
narrative_ontology:cs_axiom_status(temporal_suspension_preserves_eternal_mandate, holdable).
narrative_ontology:cs_axiom_grounding('e5177463-a128-4fa4-a389-507fd498f68b', temporal_suspension_preserves_eternal_mandate, conventional).
narrative_ontology:cs_axiom('e5177463-a128-4fa4-a389-507fd498f68b', secondary, institutional_survival_justifies_doctrinal_suspension).
narrative_ontology:cs_axiom_status(institutional_survival_justifies_doctrinal_suspension, holdable).
narrative_ontology:cs_axiom_grounding('e5177463-a128-4fa4-a389-507fd498f68b', institutional_survival_justifies_doctrinal_suspension, instrumental).
narrative_ontology:cs_reference_frame('e5177463-a128-4fa4-a389-507fd498f68b', prophetic_revelation_governs_doctrine).
narrative_ontology:cs_drift_state('e5177463-a128-4fa4-a389-507fd498f68b', post_1890_federal_legal_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e5177463-a128-4fa4-a389-507fd498f68b', '2026-07-25T14:32:18Z').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, lds_institutional_leadership).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_lds_membership).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_practitioners).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Church leadership, principally the President who authors the Manifesto, declares the new prophetic direction: plural marriage is suspended (not abrogated) to preserve the institutional church's ability to operate legally in the United States, maintain temple access, and continue missionary work. They control the narrative framing—this is prophetic reinterpretation, not capitulation to federal coercion. They enforce excommunication of practitioners who continue polygamy, effectively suspending their membership and temple privileges. The leadership benefits from institutional continuity and restored standing with civil authorities.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, lds_institutional_leadership, agenda_setter,
    institutional, generational, mobile, national).

% The broader church membership—those who accept the Manifesto as legitimate prophetic direction—gain relief from legal persecution, ability to travel and conduct business without federal prosecution, and continued access to church ordinances and social belonging. They endorse the leadership's framing and accept the suspension as divinely authorized. Their coordination benefit is substantial: legal safety and institutional coherence in place of the previous era of legal jeopardy.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_lds_membership, beneficiary,
    moderate, generational, constrained, national).

% Church members and households practicing plural marriage who interpret the original 1835 revelation as binding and eternal—not subject to suspension or temporal modification. They refuse to abandon their marriages or accept the Manifesto as legitimate prophetic direction. They experience excommunication, loss of temple privileges, institutional expulsion, and community ostracization. The constraint forces them to choose between marriage (their lived commitment and identity) and institutional belonging; they remain religiously and socially trapped in their plural households but are severed from the church structure. Over time, this group fragments into independent fundamentalist communities.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_practitioners, payer,
    powerless, biographical, identity_locked, local).

% The existing households in plural marriage at the 1890 cutoff—wives, children, families whose entire life structure is organized around the practice the Manifesto suspends. They bear the practical and emotional cost of institutional severing: loss of sacramental standing (no temple worship, no sealing of family bonds in the church's cosmology), community expulsion, and economic precarity as the institutional church (which had organized land, cooperative businesses, and social welfare) withdraws support from polygamist families. Their situation is trapped rather than merely constrained—alternatives (entering monogamous marriage, leaving the region) are available in abstract form but carry identity-destruction costs.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_households, payer,
    powerless, biographical, trapped, local).

% The U.S. federal government—explicitly Congress and the courts pursuing the Edmunds Act (1882) and Edmunds-Tucker Act (1887)—is the external coercive power that creates the legal pressure that makes the Manifesto's institutional survival justification plausible. Federal authorities are excluded from the church's own legitimacy narrative: the Manifesto frames the change as endogenous (prophetic reinterpretation), not as capitulation to external force. If federal pressure were acknowledged as the driver, the prophetic framing would collapse into exogenous override or institutional pragmatism readings.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, federal_government, excluded,
    institutional, biographical, mobile, national).

% Practitioners and theological interpreters who maintain that the 1890 Manifesto is not prophetic reinterpretation but institutional betrayal of doctrine. They would argue that the Manifesto is coerced institutional pragmatism dressed in revelation language, and that the original revelation (D&C 132, 1835) remains binding. They are structurally excluded from this reading's authority structure: the church's institutional framing defines away their interpretive standing as sectarian or fundamentalist rather than doctrinal.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_interpreters, excluded,
    powerless, biographical, identity_locked, local).

% External historians, religious scholars, and policy analysts who examine the constraint without institutional stake. They can document the sequence of federal legal pressure, the institutional leadership's rationale for the Manifesto, the doctrinal language of suspension vs. abrogation, and the lived experiences of polygamist families excommunicated under the new regime. They observe the constraint from outside the church's authority structure and can assess whether the endogenous reinterpretation framing withstands scrutiny or collapses into pragmatic response to coercion.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__endogenous_reinterpretation_reading, lds_institutional_leadership).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Manifesto coordinates the Church's entire institutional membership around acceptance of a reinterpretation of the plural marriage mandate: the practice is suspended (not abrogated) to preserve institutional survival, while doctrine remains unchanged. This enables legal operation, temple access, missionary work, and institutional coherence. The coordination problem solved is preventing institutional fracture between those accepting the new prophetic direction and those maintaining the original interpretation.
% TRANSFER_FUNCTION: The constraint transfers institutional belonging, sacramental standing (temple access, sealing ordinances), and community standing from plural marriage practitioners and households to mainstream membership accepting the Manifesto. Concretely: excommunication removes polygamists from the institutional structure while preserving mainstream members' participation. The church institution itself transfers from legal jeopardy (property confiscation, prosecution) to legal standing and institutional stability.
% ABSENT_VOICES: Federal authorities whose legal pressure created the institutional survival imperative are absent from the Manifesto's legitimacy narrative—making the external coercive driver an unacknowledged absent voice. Fundamentalist interpreters who maintain the original reading as eternally binding are structurally excluded by the church's institutional framing: their reading is delegitimized as 'sectarian' or 'fundamentalist' rather than considered as legitimate theological disagreement. A prophetic reinterpretation claim depends on excluding alternative readings as non-prophetic, so the institutional structure necessarily silences the absent voices that would contest the reinterpretation's legitimacy.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the Church would fractionally split into compliant and fundamentalist wings (which it did historically, in actual outcomes, with the fundamentalist FLDS communities). Federal legal pressure would resume and intensify without institutional capitulation, and the Church's missionary, temple, and charitable operations would face renewed legal disability. The absence of the Manifesto's authority and enforcement would require the Church to choose between legal survival and doctrinal continuity—forcing a rearrangement of the entire institutional structure.
% FOUNDING_PROBLEM: The Church faces legal annihilation under federal polygamy laws (Edmunds Act 1882, Edmunds-Tucker Act 1887): property confiscation, prosecutions, and institutional disability. The founding problem is institutional survival under legal assault. From the endogenous reinterpretation reading, the founding problem also has a theological dimension: the Church's prophetic authority is questioned if it cannot clarify the binding status of plural marriage (eternal vs. temporal) through continued revelation.
% FOUNDING_PROBLEM_CORROBORATION: Church leadership (President Woodruff and the apostolic council) attests the founding problem and its resolution via revelation. Mainstream members largely accept this account. Federal authorities (Congress, courts, the Executive) attest that legal coercion drove the institutional change—their legislative records and legal proceedings document the pressure that created the 'survival' imperative. Independent historians and scholars document both the federal pressure and the Church leadership's response narrative. Fundamentalist practitioners and interpreters attest that the founding problem is institutional survival under federal pressure, not a theological problem susceptible to prophetic resolution—they view the revelation claim as post-hoc legitimation rather than as addressing an inherent doctrinal ambiguity. No party outside the Church's institutional leadership corroborates the revelation narrative itself; the revelation claim is asserted only by the leadership and accepted by mainstream members. The corroboration asymmetry is structural: if the revelation were objectively verifiable, it would not require faith; the institutional leadership's testimony IS the only evidence available for the revelation claim.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__endogenous_reinterpretation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(plural_marriage_mandate__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).
:- end_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.38 (end of interval) because the constraint asymmetrically redistributes institutional standing: beneficiaries (mainstream members, institutional leadership) gain legal safety and sacramental standing; victims (fundamentalists) lose both. The extraction is real but moderate rather than maximal because the framing as prophetic reinterpretation makes it coordination-adjacent: the mainstream membership genuinely benefits from institutional survival and accepts the theological resolution as legitimate, not coercive. Suppression is higher (0.62) because the constraint's persistence depends on active enforcement: excommunication of practitioners, expulsion from church structures, and the delegitimization of the fundamentalist reading as non-prophetic. The high suppression reflects the institutional machinery required to hold the line against practitioners who refuse the Manifesto. Theater is elevated (0.58) because the constraint's legitimacy narrative—prophetic reinterpretation—carries theatrical elements: the revelation claim is never directly demonstrated to external parties; its authority rests on institutional declaration rather than evidence. The rise in theater from 1888 to 1890 (0.38 to 0.58) marks the moment the Manifesto is issued and the revelation narrative becomes the sole authorized account. The slight decline afterward (0.58 to 0.59) reflects the sustained theatrical maintenance of the revelation framing as federal legal pressure eases (after Utah statehood in 1896) and the constraint's economic function becomes more about institutional control than survival necessity. Measurements are authored on a single shared time grid: every metric has a value at every examined time point (1880, 1888, 1890, 1900, 1910, 1920) so the engine has no missing-value gaps. The accessibility_collapse (0.71) reflects the constraint's completeness: once the church institution has declared the Manifesto prophetic and begun excommunication, alternatives for fundamentalists narrow to apostasy or clandestine practice—both carry identity-destruction or legal risk. Resistance (0.69) is elevated because the constraint meets substantial opposition: fundamentalist practitioners actively resist, maintaining separate communities and continuing plural marriage; fundamentalist interpreters contest the legitimacy of the Manifesto as prophetic reinterpretation. The constraint persists despite real resistance because institutional machinery (excommunication, property control, social pressure) enforces it.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (institutional leadership) and the payer seats (fundamentalist practitioners) should diverge sharply in their computed type. From the institutional leadership's position, the constraint is rope: genuine coordination around a divinely sanctioned reinterpretation that preserves institutional capacity and resolves theological tension. From the fundamentalist seat, the same structure operates as enforced extraction: coerced abandonment of doctrine, institutional expulsion, and identity destruction through the mask of prophetic reinterpretation. The engine computes these divergences from the structural data (beneficiary vs. victim, power asymmetry, exit options, enforcement requirements), not from authored type claims. The commentary here explains why the two seats should produce different classifications: they have inverted directionality relationships to the constraint, and their power asymmetry (institutional vs. powerless) ensures the machinery of enforcement runs in one direction.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership and mainstream membership are beneficiaries (d near 0.0): they collect legal safety, institutional continuity, and sacramental standing. The mainstream membership has moderate power and constrained exit (they accept the theological resolution and gain coordination benefit, so their d moderates toward symmetric). Fundamentalist practitioners and plural marriage households are victims (d near 1.0): they lose institutional standing, sacramental access, and community belonging; they are identity-locked (the choice to leave their marriages carries identity destruction) so they cannot easily exit despite high d. The institutional leadership holds powerful and mobile exit options (they could theoretically reject the constraint, but institutional coherence pressures make that move costly)—their d is modulated downward by their bargaining power and ability to shape the narrative. Federal authorities, who created the coercive pressure, are excluded from this constraint story's authority structure—their role is recognized structurally (as the absent voice that makes institutional survival reasoning plausible) but not modeled as a stakeholder with directionality. The fundamentalist interpreters are excluded by institutional fiat: their reading is delegitimized as non-prophetic, so they have no recognized directionality within this constraint's authority frame (from exogenous_override_reading or institutional_pragmatism_reading, their directionality would be different).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is originally the revelation of plural marriage (D&C 132, 1835): establish the practice as eternally binding. The 1890 Manifesto, under this reading, does not abrogate that mandate but reinterprets its temporality: plural marriage remains eternally doctrinal but is temporarily suspended. This framing preserves mandatrophy avoidance: the original mandate is not abandoned, merely dormant. However, the structural evidence of mandatrophy is substantial. The founding problem (institutional survival under federal legal assault) is acute in 1880–1890 and substantially resolved after Utah statehood in 1896 and the turn of the 20th century. By 1910–1920, the acute legal jeopardy is gone, yet the Manifesto's enforcement (excommunication of polygamists) persists unchanged. The rising theater_ratio (from 0.22 in 1880 to 0.58 in 1890, sustained at 0.59–0.61 after 1900) indicates that the constraint's operational justification has increasingly shifted from coordination (solving the institutional survival problem) to theatrical maintenance of the prophetic narrative itself. From the institutional pragmatism reading, this is clear mandatrophy: the constraint outlived its function (institutional survival) and now persists as institutional inertia dressed in revelation rhetoric. From the endogenous reinterpretation reading, the constraint remains permanent (because plural marriage's suspension is eternally mandated, not temporary in any functional sense), so mandatrophy in the functional sense does not apply—the constraint correctly reflects ongoing prophetic authority. The classification logic here is: endogenous reinterpretation reading => constraint is permanent by design (mandate has evolved, not died) => no mandatrophy verdict. The contradictory temporal status (the Manifesto declares suspension, not abrogation, yet the constraint persists indefinitely) is routed to omega variables as a conceptual ambiguity about the meaning of prophetic reinterpretation itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_revelation_vs_institutional_pragmatism,
    'Is the 1890 Manifesto a legitimate prophetic reinterpretation of the plural marriage mandate (endogenous reading), or is the revelation narrative instrumental cover for institutional pragmatism responding to federal coercion (pragmatism reading)?',
    'This ambiguity cannot be resolved empirically—the revelation claim is not directly observable. The resolution hinges on interpretive assessment: (1) Internal consistency: does the theological framing of ''temporal suspension'' coherently preserve the original mandate? (2) Institutional motive analysis: what would the leadership have done absent federal pressure? (3) Textual exegesis: does the original revelation language support a temporal-suspension reading? (4) Lived outcome analysis: do fundamentalist believers—those with the most stake in the theological truth of the reading—accept the Manifesto as legitimate reinterpretation or treat it as betrayal?',
    'If the endogenous reinterpretation reading is true, the constraint type is rope (coordination around legitimate prophetic direction) and the extraction is moderate, justified by institutional survival necessity. If the pragmatism reading is true, the constraint type is snare (coercion disguised as theology) and the extraction is higher, unjustified by coordination. The temporal status of plural marriage (eternally binding or eternally subject to suspension) hinges on which reading is adopted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(endogenous_revelation_vs_institutional_pragmatism, conceptual, 'Whether the Manifesto instantiates endogenous theological reinterpretation or pragmatic institutional adaptation disguised as revelation').

omega_variable(
    federal_coercion_role_in_reinterpretation,
    'To what extent did federal legal pressure (Edmunds Act, Edmunds-Tucker Act) causally drive the Manifesto''s timing and form, versus enabling or accelerating a reinterpretation that would have occurred independently?',
    'Comparative historical analysis: (1) Church leadership writings and private correspondence before and after 1890, examining whether the revelation claim appears in pre-Edmunds contexts or emerges only under legal pressure. (2) Institutional counterfactual: what would the church''s trajectory have been absent federal pressure (speculative but informed by institutional statements and doctrinal logic). (3) Timing analysis: does the Manifesto appear at the moment of maximum federal pressure, or at a point where the legal jeopardy had begun to ease?',
    'If federal pressure was the primary driver (high causal role), the endogenous reinterpretation reading becomes instrumentally fragile—the revelation timing appears reactive rather than prophetic. If federal pressure was enabling but not driving (lower causal role), the endogenous reading is structurally stronger: revelation could have occurred absent coercion. The directionality of the fundamentalist victims also shifts: if federal pressure drove the change, the fundamental targets are federal policy (external coercion), not the leadership''s reinterpretation; if the reinterpretation is independent, the leadership becomes the primary agent of extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_coercion_role_in_reinterpretation, empirical, 'Causal role of federal legal pressure in the Manifesto''s authorship and timing').

omega_variable(
    suspension_vs_abrogation_theological_coherence,
    'Is the theological distinction between suspension (temporary, revocable) and abrogation (permanent, final) coherent within the church''s doctrinal framework, or does the Manifesto''s indefinite enforcement of the suspension effectively constitute abrogation regardless of the framing?',
    'Doctrinal exegesis and subsequent church teaching: (1) Does the church''s subsequent theology (pre-1990, when D&C 132 polygamy verses were removed from the Standard Works; post-2019, when church teachings formally disavowed polygamy) support an interpretation of the Manifesto as permanent abrogation or as genuinely temporary suspension? (2) Official statements: has the church leadership ever hinted that plural marriage could be restored, or has it consistently treated the suspension as functionally permanent? (3) Fundamentalist counterargument: does the fundamentalist reading (suspension is temporary, awaiting future restoration) remain within the doctrinal bounds of the church''s own theology, or does it require external theological invention?',
    'If suspension is coherent and distinct from abrogation, the endogenous reinterpretation reading can claim doctrinal consistency: the original mandate persists, only the practice is suspended. If suspension collapses into abrogation under scrutiny, the endogenous reading becomes strategically incoherent—the theological distinction between suspension and abrogation is a cover for permanent abandonment. The mandatrophy analysis also shifts: a truly temporary suspension would carry a sunset clause; the Manifesto''s indefinite enforcement without a stated restoration date suggests mandatrophy (the suspension outlived its function) or the incoherence of the suspension framing itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suspension_vs_abrogation_theological_coherence, conceptual, 'Theological coherence of the suspension vs. abrogation distinction').

omega_variable(
    fundamentalist_reading_as_legitimate_alternative,
    'Is the fundamentalist reading (the original D&C 132 revelation binds eternally and cannot be suspended by later declaration) a legitimate interpretation of the church''s own doctrine, or is it sectarian innovation requiring external doctrinal authority?',
    'Doctrinal analysis and genealogy: (1) Can fundamentalists substantiate their reading from the original revelation text and foundational church teachings without importing post-1890 assumptions? (2) Does the church''s own theological framework—before the Manifesto—contain resources (revelation language, prophetic authority structures, precedents for reinterpretation) that would allow a pre-Manifesto reader to expect either permanence or temporality? (3) Institutional genealogy: how did the church''s authority structures evolve to make the Manifesto''s reinterpretation possible—was it a novel move or an extension of existing precedents?',
    'If the fundamentalist reading is legitimate within the church''s pre-1890 doctrine, then the Manifesto represents a genuine doctrinal choice and split, not a correction of error. The endogenous reinterpretation reading then becomes one reading among legitimate alternatives, not the unique true reading. The constraint becomes a power struggle over legitimate doctrine, not a coordination around truth. If the fundamentalist reading is sectarian (requires post-1890 assumptions or external authority), the endogenous reading becomes structurally stronger: the Manifesto corrects misinterpretation, not betrayal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamentalist_reading_as_legitimate_alternative, conceptual, 'Doctrinal legitimacy of the fundamentalist reading within the church''s pre-1890 authority framework').

omega_variable(
    reading_committer_structure,
    'This constraint instantiates one of three readings of the plural_marriage_mandate kernel. The three readings (endogenous_reinterpretation, exogenous_override, institutional_pragmatism) are structurally distinct: they would produce different constraint types, beneficiary sets, and extraction profiles if separately authored. Is the ambiguity about which reading is correct resolvable by the evidence, or is it irreducible by design (a permanent feature of the kernel''s contestation)?',
    'Meta-analysis of the committer structure itself: (1) What evidence would adjudicate between the readings? (2) Is the evidence available or is the disagreement located in non-empirical premises (theological authority claims, institutional legitimacy)? (3) Do the parties (church leadership, fundamentalists, federal authorities) have an incentive to conceal or suppress evidence that would resolve the ambiguity?',
    'If the ambiguity is resolvable, then the corpus should contain three constraint stories (one per reading) with divergent metrics and classifications, and the evidence analysis should indicate which reading withstands scrutiny. If the ambiguity is irreducible, then all three readings remain live, and the constraint story instantiates the endogenous reading as one live option, not as the truth. This affects the status of the reading itself: is it a detected error in an alternative reading, or is it a legitimate interpretation that must coexist with alternatives?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_committer_structure, preference, 'Meta-question about the committer structure: is the reading ambiguity resolvable or irreducible?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__endogenous_reinterpretation_reading, 1880, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1880, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1880, 0.22).
narrative_ontology:measurement(plur_tr_t1888, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1888, 0.38).
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.58).
narrative_ontology:measurement(plur_tr_t1900, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1900, 0.61).
narrative_ontology:measurement(plur_tr_t1910, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1910, 0.59).
narrative_ontology:measurement(plur_tr_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1920, 0.58).

% Extraction over time
narrative_ontology:measurement(plur_be_t1880, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1880, 0.18).
narrative_ontology:measurement(plur_be_t1888, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1888, 0.28).
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.42).
narrative_ontology:measurement(plur_be_t1900, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement(plur_be_t1910, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1910, 0.35).
narrative_ontology:measurement(plur_be_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1920, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1880, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1880, 0.35).
narrative_ontology:measurement(plur_su_t1888, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1888, 0.48).
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.62).
narrative_ontology:measurement(plur_su_t1900, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1900, 0.68).
narrative_ontology:measurement(plur_su_t1910, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1910, 0.61).
narrative_ontology:measurement(plur_su_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1920, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.12).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% The 'plural_marriage_mandate' kernel decomposes into three structurally distinct readings (three separate constraint stories), each with its own ε, beneficiary/victim set, and constraint type. The endogenous_reinterpretation_reading (this story) claims the Manifesto is legitimate prophetic reinterpretation (rope type, moderate extraction). The exogenous_override_reading claims federal coercion overrides divine revelation (snare type, high extraction). The institutional_pragmatism_reading claims institutional adaptation disguised as theology (snare type, high extraction with incoherent framing). All three share the kernel (the plural marriage mandate itself, D&C 132) but produce fundamentally different classifications. The readings are linked via network.affects_constraints to indicate they are alternative instantiations of the same kernel, not independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
