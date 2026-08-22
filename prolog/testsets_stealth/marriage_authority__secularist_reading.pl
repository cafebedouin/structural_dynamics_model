% ============================================================================
% CONSTRAINT STORY: marriage_authority__secularist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__secularist_reading, []).

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
 *   constraint_id: marriage_authority__secularist_reading
 *   human_readable: Uniform Civil Code Program: Exclusive Legislative Authority over Marriage
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   In a constitutionally secular state with religiously segmented family
 *   law, a governing coalition pursues a Uniform Civil Code: one
 *   legislatively authored code of marriage, divorce, and inheritance for all
 *   citizens, replacing community-administered personal law regimes. The
 *   majority community was placed under codified family law decades ago;
 *   minority communities retain uncodified communal law, and the program's
 *   demand for uniformity therefore lands asymmetrically — the majority has
 *   already paid the adaptation cost, and the proposed code is written
 *   largely in its image. The program carries a genuine coordination function
 *   (conflicts of law, forum shopping, registration) and a genuine equality
 *   warrant (some communal provisions disadvantage women), while transferring
 *   law-authorship from permanently outvoted communities to a legislature
 *   they cannot swing. Modeled on the Indian UCC trajectory (Article 44
 *   directive principle, 1950; Hindu Code Acts, 1955-56; Shah Bano and Sarla
 *   Mudgal episodes; state-level codes from 2024). Claim and metrics are
 *   authored independently: the claimed type is tangled_rope because both
 *   coordination and asymmetric extraction are structurally present; the
 *   metrics describe the program's actual operation. Committer structure
 *   (kernel, reading, siblings) is recorded in kernel_context and the omega
 *   variables, not folded into the constraint itself.
 *
 * KEY AGENTS:
 *   - secular_modernist_coalition: agenda-setter (institutional/arbitrage) — drafts the code, controls the legislative calendar, collects the authorship role and integration dividends
 *   - codified_majority_community: primary beneficiary (powerful/mobile) — already codified, adapts minimally, supplies decisive voting weight
 *   - minority_religious_communities: primary target (organized/identity_locked) — lose communal law-authorship to a legislature they cannot control
 *   - minority_personal_law_institutions: target (moderate/trapped) — councils and forums whose jurisdiction and purpose end with the code
 *   - women_within_minority_communities: dual-positioned seat (moderate/constrained) — bear forum-loss costs, may gain textual equality, outcome depends on code content
 *   - tribal_customary_regions: excluded (organized/constrained) — exempted rather than consulted; extension remains on the agenda
 *   - internal_reform_movements: excluded (moderate/constrained) — the internal-reform middle path is foreclosed by the binary framing
 *   - constitutional_courts: analytical observer (institutional/analytical) — sees the full structure, authors nothing, bears no compliance burden
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__secularist_reading, 0.74).
domain_priors:suppression_score(marriage_authority__secularist_reading, 0.65).
domain_priors:theater_ratio(marriage_authority__secularist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__secularist_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__secularist_reading, "Uniform Civil Code Program: Exclusive Legislative Authority over Marriage").
narrative_ontology:topic_domain(marriage_authority__secularist_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__secularist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__secularist_reading, '48273117-0654-46b9-8dbf-661b793171db').
narrative_ontology:cs_kernel_codification('48273117-0654-46b9-8dbf-661b793171db', formalized).
narrative_ontology:cs_authority_grounding('48273117-0654-46b9-8dbf-661b793171db', lineage).
narrative_ontology:cs_interpretation_layer_present('48273117-0654-46b9-8dbf-661b793171db').
narrative_ontology:cs_reading_relation('48273117-0654-46b9-8dbf-661b793171db', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('48273117-0654-46b9-8dbf-661b793171db', marriage_authority__federalist_millet_reading, forecloses).
narrative_ontology:cs_reading_relation('48273117-0654-46b9-8dbf-661b793171db', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('48273117-0654-46b9-8dbf-661b793171db', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('48273117-0654-46b9-8dbf-661b793171db', foundational, exclusive_democratic_marriage_authority).
narrative_ontology:cs_axiom_status(exclusive_democratic_marriage_authority, holdable).
narrative_ontology:cs_axiom_grounding('48273117-0654-46b9-8dbf-661b793171db', exclusive_democratic_marriage_authority, conventional).
narrative_ontology:cs_axiom('48273117-0654-46b9-8dbf-661b793171db', foundational, legal_pluralism_transitional_anomaly).
narrative_ontology:cs_axiom_status(legal_pluralism_transitional_anomaly, holdable).
narrative_ontology:cs_axiom_grounding('48273117-0654-46b9-8dbf-661b793171db', legal_pluralism_transitional_anomaly, empirically_contingent).
narrative_ontology:cs_axiom('48273117-0654-46b9-8dbf-661b793171db', secondary, uniform_citizenship_requires_uniform_family_law).
narrative_ontology:cs_axiom_status(uniform_citizenship_requires_uniform_family_law, holdable).
narrative_ontology:cs_axiom_grounding('48273117-0654-46b9-8dbf-661b793171db', uniform_citizenship_requires_uniform_family_law, deontological).
narrative_ontology:cs_reference_frame('48273117-0654-46b9-8dbf-661b793171db', unitary_legislative_family_law_supremacy).
narrative_ontology:cs_drift_state('48273117-0654-46b9-8dbf-661b793171db', contemporary_pluralist_entrenchment, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('48273117-0654-46b9-8dbf-661b793171db', '').
narrative_ontology:cs_kernel_id(marriage_authority__secularist_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, codified_majority_community).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, minority_religious_communities).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, minority_personal_law_institutions).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, women_within_minority_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, women_within_minority_communities).
narrative_ontology:constraint_vindicates(marriage_authority__secularist_reading, national_integration_through_legal_uniformity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A cross-party bloc of legislators, jurists, and reform intellectuals that drafts and promotes a single civil code covering marriage, divorce, and inheritance for all citizens. Controls the parliamentary calendar on family-law bills, writes the code's text, and campaigns on national integration and equal citizenship. Bears essentially no adaptation cost because the code is written in terms its members already live under, and collects the authorship role itself along with the electoral and symbolic dividends of the integration project.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, secular_modernist_coalition, agenda_setter,
    institutional, generational, arbitrage, national).

% The largest religious community, whose family relations were placed under a legislatively codified law in the 1950s. Having already accepted legislative authorship of its marriage rules, it experiences the proposed single code as long-delayed symmetry: the remaining uncodified communities would come under the same kind of regime it submitted to a lifetime ago. Adapts minimally because successive draft codes borrow heavily from its existing codified norms, and supplies the voting weight that makes legislative authorship decisive.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, codified_majority_community, beneficiary,
    powerful, generational, mobile, national).

% Religious minority communities whose marriage, divorce, and inheritance continue to run on uncodified communal law administered by their own scholars and councils. A single code authored by a legislature in which they are a permanent voting minority would replace norms they recognize as their own with text they did not write and cannot amend through their institutions. Emigration is costly and rare, and treating the code as merely one more statute to comply with would mean surrendering the legal boundary that marks the community as a distinct people.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, minority_religious_communities, payer,
    organized, generational, identity_locked, national).

% Community councils, seminaries, and arbitral forums that currently decide marriage, divorce, and maintenance disputes under communal law. A single code transfers their docket to state courts and renders their rulings unenforceable; their institutional purpose ends with their jurisdiction. They hold no forum in which their continued existence can be negotiated.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, minority_personal_law_institutions, payer,
    moderate, biographical, trapped, regional).

% Women living under minority communal family law. Some of its provisions disadvantage them in divorce, maintenance, and inheritance, and a uniform code could remedy that; at the same time, displacing communal forums removes dispute venues that are geographically and culturally accessible to them, and the replacement code is drafted by a legislature responsive to the majority community, so its text may embed different disadvantages. Their net position depends on code content they do not control.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, women_within_minority_communities, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__secularist_reading, women_within_minority_communities, beneficiary).

% Hill and tribal regions whose customary family arrangements are currently shielded by constitutional carve-outs and schedule protections. Draft proposals routinely exempt them rather than consult them; their consent is deferred, not sought, and their customary authorities hold no seat in the drafting process even though extending the code to them remains on the long-term agenda.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, tribal_customary_regions, excluded,
    organized, generational, constrained, regional).

% Reformers working inside minority traditions — scholars, women's groups, and clerics arguing for reinterpretation of communal law from within. The public framing offers only two positions, retaining communal law as-is or replacing it wholesale with a single code; the middle path of internal reform is thereby starved of legislative attention and resources, since it suits neither the defenders of the status quo nor the proponents of replacement.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, internal_reform_movements, excluded,
    moderate, biographical, constrained, national).

% Apex and high courts that adjudicate conflicts between communal law and constitutional guarantees. They periodically urge legislative action toward a single code in obiter remarks while applying constitutional floors case by case; they see the full structure across all communities but author nothing and bear no compliance burden.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:fixing_cost_class(marriage_authority__secularist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single family code solves real conflicts-of-law problems: couples marrying across community lines currently face incompatible regimes, forum shopping between parallel personal-law systems distorts divorce and maintenance outcomes, and the state cannot administer marriage registration uniformly. One code gives every citizen identical civil status and removes jurisdictional arbitrage.
% TRANSFER_FUNCTION: Moves law-authorship over marriage and family from community religious institutions to the central legislature; moves the compliance and adaptation costs of the new code onto the communities whose norms deviate furthest from it; and consolidates the symbolic-integration and electoral gains of the project to the governing coalition and the majority community whose codified norms supply most of the code's content.
% ABSENT_VOICES: Internal reform movements and tribal customary authorities are structurally sidelined: the dominant framing recognizes only retention or wholesale replacement, so the internal-reform middle path never reaches the drafting table, and tribal regions are exempted rather than consulted. Women inside minority communities are present but their advocacy is routed through either community leadership or secularist organizations, leaving no independent seat for positions that fit neither camp.
% DISAPPEARANCE_RATIONALE: If the single-code program vanished overnight, the plural status quo would simply continue — but the political field would rearrange: minority politics would lose its principal defensive mobilization axis, the majority community would lose its symmetry argument for further reform, the courts would lose the harmonization docket that currently substitutes for legislation, and the governing coalition would lose a core nation-building commitment around which its identity is organized.
% FOUNDING_PROBLEM: Post-colonial state formation inherited family law segmented by religious category under colonial rule. The founding problem was building a single civic identity: giving every citizen the same civil status, ending conflicts of law when citizens married across community lines, and removing provisions in religious family codes that disadvantaged women.
% FOUNDING_PROBLEM_CORROBORATION: Partially corroborated from outside the benefiting parties: apex-court observations on the hardships of conflicting personal laws are on the record, and law-reform commission consultations document the conflicts-of-law problem. But the specific claim that pluralism is a transitional anomaly rather than a durable feature of a diverse democracy is attested almost exclusively by the coalition itself; minority-community submissions to the consultation processes, and the reform commission's own 2018 consultation paper, dispute the anomaly framing. No source outside the benefiting parties attests that elimination of pluralism is required or imminent.
narrative_ontology:disappearance_verdict(marriage_authority__secularist_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__secularist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__secularist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__secularist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__secularist_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__secularist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__secularist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__secularist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.74: the program's core operation is a transfer of law-authorship from communities to a legislature structurally controlled by the majority — the costs concentrate on those least able to influence the text, and the transfer compounds as each new provision narrows communal jurisdiction further. Suppression 0.65 is structural, not interpersonal: statutory displacement, mandatory civil registration, and invalidation of communal rulings require active enforcement machinery, and the machinery grows as implementation spreads (suppression_requirement series rises 0.15 to 0.65). Theater_ratio 0.30: the equality warrant is partly real — gender-disadvantageous provisions exist in some communal codes — but a growing share of program rhetoric defends uniformity for its own sake and cites majority sacrifice from a half-century ago; the declining series (0.50 to 0.30) tracks function replacing the directive-principle-era pure declaration. Accessibility_collapse 0.55: where the code operates, legal alternatives collapse completely, but informal communal practice persists alongside and national rollout is incomplete, so alternatives are narrowed, not extinguished. Resistance 0.68: organized minority political resistance, won regional carve-outs, and reform-commission pushback are sustained and effective at the margin. All three tracked series run on one shared seven-point grid (t=0,15,30,45,60,70,76); trajectories are monotonic — a political-salience ratchet, not a cycle — so no cyclical-pattern machinery is invoked. No directionality_overrides are authored: the derivation chain produces accurate d values from the declared roles and exits, and the override mechanism keys on power atoms, which would misapply across the three distinct moderate-power seats.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different types from identical constitutional text. From the agenda-setter seat the program is the completion of nation-building: a rope it built, delayed only by recalcitrant minorities. From the codified-majority seat it is belated symmetry — a fair deal it already accepted. From the minority-community and communal-institution seats the same structure operates as majoritarian dispossession of law-authorship, enforced by a state they did not author and cannot amend. The women's seat splits internally on code content. The courts see a manageable conflicts docket. These divergences follow from the structural data — power, exit options, and declared position — and the engine computes them; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: the coalition (agenda-setter, arbitrage exit — it writes the rules in its own idiom) sits nearest the beneficiary end; the codified majority (beneficiary, mobile — the code borrows its norms) sits nearby. Victim declarations drive high d: minority communities (identity_locked — communal law is a constitutive boundary marker of communal existence, so exit means identity dissolution, pushing d toward the full-target end beyond what mere trapping would) and communal institutions (trapped — no forum negotiates their survival). Women within minority communities carry a dual declaration; the derivation reads them target-side from the victims listing, with the true position oscillating on code content — handled by the gender_outcome_distribution omega rather than an override, since overrides key on power atoms and would contaminate the other moderate seats. Courts are analytical and neutral. Spatial scope is national: verifying uniform application across a continent-scale polity is genuinely hard, so the engine's scope amplification applies modestly to effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — post-colonial civic integration and cross-community conflicts of law — is partly live: interfaith couples still face incompatible regimes, and forum shopping is real. But the specific premise that pluralism is a transitional anomaly is seventy-five years stale as a prediction and contested as a description, and the program's persistence is now carried substantially by coalition identity and electoral payoff rather than by integration urgency. The tangled_rope classification guards against both misreadings: a pure-rope reading (uniformity as neutral good) erases the asymmetric authorship transfer; a pure-snare reading (majoritarian land-grab) erases the real coordination service delivered to interfaith and mobile citizens. Mandatrophy risk runs forward: if the founding problem is judged dead while the program persists on rhetoric alone, the arrangement decays toward piton — annual directive-principle invocations with no operational content. Current state-level implementation keeps the function real, which is why mandatrophy is flagged as a trajectory, not a verdict. The R5 interview records the status as contested with corroboration only partial and partly coalition-internal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the marriage_authority kernel (secularist_reading). Which structural elements would the sibling readings relocate, and where exactly is the disagreement located?',
    'Not empirically resolvable; resolved per-framework by which reading a party adopts. The disagreement is located in two elements: the locus of legitimate law-authorship (legislature versus community versus courts) and the normative status of pluralism (transitional defect versus deliberate design). Documented via cs_structure.reading_relations and axioms.',
    'Under communal_autonomy_reading the beneficiary/victim sets invert — communities become the beneficiaries of retained authorship and the legislature the usurping party. Under federalist_millet_reading pluralism is protective design, collapsing the extraction claim entirely. Under gender_rights_reading the victim set becomes patriarchal provisions rather than communities. Each sibling is a separate constraint story with its own epsilon; this file''s values are valid only for the secularist reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer-frame routing: one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    majoritarian_code_content,
    'Does the uniform code''s substantive content track neutral equality principles, or does it reproduce the majority community''s codified norms under a neutral label?',
    'Clause-level comparative analysis of successive draft codes against all existing personal-law regimes, scoring divergence of each provision from every community''s current law; provisions clustering around one community''s norms indicate majoritarian content.',
    'If the code is majoritarian in content, measured extraction rises above the authored 0.74 and the coordination claim weakens — uniformity in one community''s image is not neutrality, and the arrangement trends toward snare. If content is genuinely convergent-neutral, the coordination function strengthens and extraction moderates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_code_content, empirical, 'Whether uniformity means neutrality or majoritarian normalization.').

omega_variable(
    gender_outcome_distribution,
    'What is the net effect of code replacement on women within minority communities — do equality gains in the code''s text outweigh the loss of accessible communal dispute forums?',
    'Longitudinal outcome study in jurisdictions where a uniform code has operated (state-level implementations): divorce, maintenance, and inheritance outcomes for women before and after cutover, controlling for access distance to forums.',
    'If net-positive, the women_within_minority_communities seat migrates toward the beneficiary end and the program''s coordination claim gains its strongest warrant; if net-negative, the seat sits at the full-target end and the equality justification is exposed as cover, raising theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_outcome_distribution, empirical, 'Net gender outcome of replacing communal forums with a majority-drafted code.').

omega_variable(
    pluralism_permanence,
    'Is personal-law pluralism genuinely a transitional stage that integration will dissolve, or a permanent structural feature of religiously diverse democracies?',
    'Comparative longitudinal data across diverse democracies: does legal pluralism in family law decay with development and integration, or does it persist and re-emerge? Seventy-five years of stasis in the reference case, with communal assertion strengthening rather than fading, is already strong evidence on the permanence side.',
    'If pluralism is permanent, the transitional-anomaly premise fails, the program''s coordination justification collapses into majoritarian preference, and the arrangement trends toward snare; the federalist_millet_reading''s design claim is vindicated. If transitional, the program''s founding problem remains live and the rope component is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pluralism_permanence, conceptual, 'The core factual premise of the secularist reading, disputed by the federalist sibling.').

omega_variable(
    authorship_versus_uniformity_objection,
    'Do minority communities object to uniformity as such, or to majoritarian authorship of the uniform text — would a code drafted by a genuinely representative body including them be accepted?',
    'Deliberative polling or structured negotiation experiments offering minority communities a code-drafting seat with veto-weight; measure acceptance rates against acceptance of the current majoritarian drafting path.',
    'If objections track authorship rather than uniformity, the extraction is located in the authorship transfer specifically — remedies short of abandonment (consociational drafting) become available and the arrangement is fixable at moderate cost. If objections track uniformity itself, the conflict is identity-constitutive and no drafting procedure resolves it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorship_versus_uniformity_objection, empirical, 'Locates the extraction: the uniformity or the authorship transfer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__secularist_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__secularist_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t15, marriage_authority__secularist_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement_basis(marr_tr_t15, observed).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__secularist_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(marr_tr_t30, observed).
narrative_ontology:measurement(marr_tr_t45, marriage_authority__secularist_reading, theater_ratio, 45, 0.39).
narrative_ontology:measurement_basis(marr_tr_t45, observed).
narrative_ontology:measurement(marr_tr_t60, marriage_authority__secularist_reading, theater_ratio, 60, 0.36).
narrative_ontology:measurement_basis(marr_tr_t60, observed).
narrative_ontology:measurement(marr_tr_t70, marriage_authority__secularist_reading, theater_ratio, 70, 0.32).
narrative_ontology:measurement_basis(marr_tr_t70, observed).
narrative_ontology:measurement(marr_tr_t76, marriage_authority__secularist_reading, theater_ratio, 76, 0.3).
narrative_ontology:measurement_basis(marr_tr_t76, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__secularist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t15, marriage_authority__secularist_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement_basis(marr_be_t15, observed).
narrative_ontology:measurement(marr_be_t30, marriage_authority__secularist_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement_basis(marr_be_t30, observed).
narrative_ontology:measurement(marr_be_t45, marriage_authority__secularist_reading, base_extractiveness, 45, 0.55).
narrative_ontology:measurement_basis(marr_be_t45, observed).
narrative_ontology:measurement(marr_be_t60, marriage_authority__secularist_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement_basis(marr_be_t60, observed).
narrative_ontology:measurement(marr_be_t70, marriage_authority__secularist_reading, base_extractiveness, 70, 0.7).
narrative_ontology:measurement_basis(marr_be_t70, observed).
narrative_ontology:measurement(marr_be_t76, marriage_authority__secularist_reading, base_extractiveness, 76, 0.74).
narrative_ontology:measurement_basis(marr_be_t76, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__secularist_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t15, marriage_authority__secularist_reading, suppression_requirement, 15, 0.22).
narrative_ontology:measurement_basis(marr_su_t15, observed).
narrative_ontology:measurement(marr_su_t30, marriage_authority__secularist_reading, suppression_requirement, 30, 0.3).
narrative_ontology:measurement_basis(marr_su_t30, observed).
narrative_ontology:measurement(marr_su_t45, marriage_authority__secularist_reading, suppression_requirement, 45, 0.42).
narrative_ontology:measurement_basis(marr_su_t45, observed).
narrative_ontology:measurement(marr_su_t60, marriage_authority__secularist_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement_basis(marr_su_t60, observed).
narrative_ontology:measurement(marr_su_t70, marriage_authority__secularist_reading, suppression_requirement, 70, 0.6).
narrative_ontology:measurement_basis(marr_su_t70, observed).
narrative_ontology:measurement(marr_su_t76, marriage_authority__secularist_reading, suppression_requirement, 76, 0.65).
narrative_ontology:measurement_basis(marr_su_t76, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__secularist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the marriage_authority kernel decomposes into five readings because the colloquial label 'who governs marriage' conflates structurally distinct claims with different epsilon referents, beneficiary sets, and failure modes. This file instantiates the secularist reading; its epsilon (0.74) describes the uniform-code program as the standing arrangement under contest. Links to the four sibling readings enable contamination propagation: each judicial-harmonization ruling that narrows communal-law variation lowers the marginal cost of formal codification and feeds this reading's program, while each successful communal-autonomy assertion raises this program's enforcement costs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
