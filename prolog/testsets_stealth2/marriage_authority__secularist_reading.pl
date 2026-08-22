% ============================================================================
% CONSTRAINT STORY: marriage_authority__secularist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Secularist Legislative-Monopoly Reading of Marriage Authority (Uniform Civil Code Telos)
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This story instantiates the secularist reading of the marriage_authority
 *   kernel: the claim that authority over marriage, divorce, maintenance, and
 *   succession belongs to the democratic legislature alone, and that
 *   personal-law pluralism is a transitional anomaly to be eliminated through
 *   a Uniform Civil Code. Modeled on the Indian Article 44 controversy and
 *   comparable post-colonial settlements, the standing arrangement under
 *   contest is the secular-uniformist program itself as an operative force in
 *   the family-law field: the seventy-five-year campaign machinery,
 *   directive-principle jurisprudence, law-commission consultation processes,
 *   and recent state-level code enactments through which legislative monopoly
 *   over marriage norms is advanced. The program coordinates (a single civic
 *   standard resolves forum-shopping between codes, unequal legal personhood
 *   across religious lines, and jurisdictional conflict) and it transfers
 *   through the same structure (normative authorship and assimilation costs
 *   concentrate on minority religious communities whose codes were never
 *   consolidated on their own terms, while the majority community's law was
 *   codified decades earlier by its own consent). Claim and metrics are
 *   authored independently: claimed_type records my structural judgment that
 *   both the coordination and the transfer halves are real; the metric values
 *   record the program's observed operation. Sibling readings of the kernel
 *   are separate constraints with their own epsilon and stakeholder
 *   structures; nothing here averages across them. KEY AGENTS (by structural
 *   relationship): - secular_modernist_coalition: Primary beneficiary
 *   (organized/identity_locked) — collects the uniformity good, electoral
 *   returns, and nation-building credit - minority_religious_communities:
 *   Primary payer (organized/trapped) — bears authorship displacement and
 *   assimilation costs - democratic_legislature: Agenda setter
 *   (institutional/arbitrage) — authors the code; electoral cycles shape
 *   timing - constitutional_courts: Reviewing seat with incidental collection
 *   (institutional/analytical) — urges the telos while imposing piecemeal
 *   floors - gender_egalitarian_reformers: Conditional beneficiary
 *   (organized/mobile) — collects standing and partial aims; alliances shift
 *   easily - tribal_customary_law_communities: Secondary payer
 *   (organized/constrained) — spends resources defending constitutional
 *   carve-outs - minority_community_women: Excluded seat (powerless/trapped)
 *   — invoked by both camps, seated by neither -
 *   comparative_family_law_scholars: Analytical observer
 *   (analytical/analytical) — supplies the comparative record every camp
 *   draws on
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__secularist_reading, 0.72).
domain_priors:suppression_score(marriage_authority__secularist_reading, 0.68).
domain_priors:theater_ratio(marriage_authority__secularist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__secularist_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__secularist_reading, "Secularist Legislative-Monopoly Reading of Marriage Authority (Uniform Civil Code Telos)").
narrative_ontology:topic_domain(marriage_authority__secularist_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__secularist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__secularist_reading, 'a14b3aad-ce5f-445a-a07c-1fe6fe20f5dd').
narrative_ontology:cs_kernel_codification('a14b3aad-ce5f-445a-a07c-1fe6fe20f5dd', fixed_text).
narrative_ontology:cs_authority_grounding('a14b3aad-ce5f-445a-a07c-1fe6fe20f5dd', lineage).
narrative_ontology:cs_interpretation_layer_present('a14b3aad-ce5f-445a-a07c-1fe6fe20f5dd').
narrative_ontology:cs_reading_relation('a14b3aad-ce5f-445a-a07c-1fe6fe20f5dd', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('a14b3aad-ce5f-445a-a07c-1fe6fe20f5dd', marriage_authority__federalist_millet_reading, coexists_with).
narrative_ontology:cs_reading_relation('a14b3aad-ce5f-445a-a07c-1fe6fe20f5dd', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('a14b3aad-ce5f-445a-a07c-1fe6fe20f5dd', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('a14b3aad-ce5f-445a-a07c-1fe6fe20f5dd', foundational, legislative_authorship_of_family_law).
narrative_ontology:cs_axiom_status(legislative_authorship_of_family_law, holdable).
narrative_ontology:cs_axiom_grounding('a14b3aad-ce5f-445a-a07c-1fe6fe20f5dd', legislative_authorship_of_family_law, conventional).
narrative_ontology:cs_axiom('a14b3aad-ce5f-445a-a07c-1fe6fe20f5dd', foundational, personal_law_pluralism_transitional_anomaly).
narrative_ontology:cs_axiom_status(personal_law_pluralism_transitional_anomaly, holdable).
narrative_ontology:cs_axiom_grounding('a14b3aad-ce5f-445a-a07c-1fe6fe20f5dd', personal_law_pluralism_transitional_anomaly, instrumental).
narrative_ontology:cs_axiom('a14b3aad-ce5f-445a-a07c-1fe6fe20f5dd', secondary, judicial_piecemeal_harmonization_insufficient).
narrative_ontology:cs_axiom_status(judicial_piecemeal_harmonization_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('a14b3aad-ce5f-445a-a07c-1fe6fe20f5dd', judicial_piecemeal_harmonization_insufficient, conventional).
narrative_ontology:cs_reference_frame('a14b3aad-ce5f-445a-a07c-1fe6fe20f5dd', directive_principle_unification_frame).
narrative_ontology:cs_drift_state('a14b3aad-ce5f-445a-a07c-1fe6fe20f5dd', contemporary_enactment_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('a14b3aad-ce5f-445a-a07c-1fe6fe20f5dd', '').
narrative_ontology:cs_kernel_id(marriage_authority__secularist_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, minority_religious_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, constitutional_courts).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, gender_egalitarian_reformers).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, tribal_customary_law_communities).
narrative_ontology:constraint_vindicates(marriage_authority__secularist_reading, uniform_citizenship_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority__secularist_reading, legislative_supremacy_in_family_law).
narrative_ontology:constraint_vindicates(marriage_authority__secularist_reading, directive_principle_completion_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A coalition of national-reform parties, constitutional modernists, rationalist associations, and urban professional classes that has campaigned since independence for a single civil code. It collects the program's returns: electoral mobilization around the code's promise, the nation-building credit of completing a founding directive, and a family-law order aligned with the civic standards its members already live under. Its members' political identity is constituted by the secular-uniformist project; abandoning the campaign would dissolve the coalition's self-understanding, so participation continues regardless of short-term prospects.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, secular_modernist_coalition, beneficiary,
    organized, generational, identity_locked, national).

% Religious minority communities — principally Muslim community institutions and personal-law boards, alongside smaller Christian and Parsi bodies — whose marriage, divorce, maintenance, and succession practices currently run under community-sanctioned codes. Under the program's completion they would trade community-authored norms for a legislatively authored code drafted without their normative input, absorbing the costs of re-aligning religious practice, clergy authority structures, and inheritance customs. Exit is unavailable: religious affiliation cannot be resigned, the territorial legal system cannot be opted out of, and emigration is not a response available to a community as such.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, minority_religious_communities, payer,
    organized, generational, trapped, national).

% The national parliament, holder of formal competence over family law under the constitutional scheme. It drafts, amends, or declines to enact the uniform code; its committees run the consultation processes and its majority decides timing. Electoral cycles shape when the code advances: the promise mobilizes majority-community voters, while enactment risks alienating minority blocs, so the legislature alternates between advancing and shelving the project across decades. It can restructure the arrangement at will and bears little of its direct cost.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, democratic_legislature, agenda_setter,
    institutional, biographical, arbitrage, national).

% Supreme and high courts adjudicate personal-law disputes, strike provisions inconsistent with constitutional guarantees, and repeatedly record lament that the uniform-code directive remains unimplemented, urging the legislature to act. A completed code would simplify a notoriously fragmented jurisprudence and reduce the courts' exposure to accusations of selective intervention, so the judiciary collects a modest administrative dividend from the program's success while formally holding the reviewing seat.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__secularist_reading, constitutional_courts, beneficiary).

% Feminist lawyers, women's-movement organizations, and egalitarian jurists who seek gender-just reform of marriage and succession law. A uniform, equality-audited code would deliver several of their substantive aims — uniform maintenance rights, restriction of unilateral divorce, equal succession — and they collect standing from both camps invoking their agenda. Many remain ambivalent: the same campaign threatens to hand community women's claims to a majoritarian instrument, so their support is conditional and their alliance-shifting is easy.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, gender_egalitarian_reformers, beneficiary,
    organized, biographical, mobile, national).

% Scheduled Tribes and Sixth Schedule communities of the northeast and central regions whose customary marriage and succession practices enjoy explicit constitutional protection. Successive draft codes have exempted them so far, but each new draft extends the code's reach toward their institutions, and they spend organizational resources defending carve-outs in every consultation round. Their protections are political bargains rather than absolutes, so their position depends on continued bargaining leverage.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, tribal_customary_law_communities, payer,
    organized, generational, constrained, regional).

% Women living under minority personal laws, whose unequal maintenance, divorce, and succession positions are the principal grievance both camps invoke. Clerical leadership negotiates on behalf of community autonomy without seating them; majoritarian reformers campaign in their name without answering to them; consultation processes reach them last, if at all. Their practical channels — constitutional litigation, internal community reform advocacy — carry social-sanction costs within their communities.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, minority_community_women, excluded,
    powerless, biographical, trapped, national).

% Academic observers of legal pluralism across post-colonial states who track how family-law authority is allocated between religious communities and state legislatures. They produce the comparative evidence on which every camp draws, hold no stake in any single state's settlement, and evaluate the arrangement against the full international record of personal-law systems.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, comparative_family_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:fixing_cost_class(marriage_authority__secularist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the legal definition of marriage, divorce, maintenance, and succession into a single territorial standard, solving the problems of conflicting personal-law jurisdictions, forum-shopping between codes, and unequal legal personhood across religious lines.
% TRANSFER_FUNCTION: Moves family-law authorship from religious and community institutions to the central democratic legislature; moves compliance and assimilation costs onto minority religious communities; moves the symbolic and electoral good of uniform national citizenship to the secular-modernist coalition.
% ABSENT_VOICES: Minority-community women — whose equality is the principal grievance invoked by both the code campaign and the community-autonomy defense — sit outside both negotiating tables: clerical leadership speaks for community autonomy, majoritarian reformers speak for gender equality, and neither seat is hers. Tribal customary authorities enter consultation processes late, after draft codes are framed. Both absences are structural: the arrangement's bargaining table is set by the legislature's majority and the coalition's organizations.
% DISAPPEARANCE_RATIONALE: If the secular-uniformist program vanished overnight, the marriage-authority field rearranges: the communal_autonomy and judicial_harmonization readings lose their principal antagonist and their principal rhetorical foil respectively, the legislature's family-law agenda loses its unifying telos, minority-community institutions lose the mobilizing threat that disciplines their internal coalitions, and gender-egalitarian reformers lose the external-leverage argument that currently anchors part of their strategy. The parties are organized around this arrangement's presence and absence alike.
% FOUNDING_PROBLEM: The post-colonial founding generation inherited family law segmented along religious lines and framed a uniform civil code as a directive principle: the intended completion of the transition from communal to civic citizenship, solving fragmented sovereignty over family life and unequal legal personhood across religious communities.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by constitutional historians documenting the founding assembly's deferred compromise on the code directive, and by minority-community jurists who acknowledge the founding generation's integrationist intent while disputing that the problem persists in its original form. The secular-modernist coalition's own attestation that the problem remains live is not independent corroboration; the communal and federalist camps attest that the founding 'problem' was misframed from the start — unity does not require uniformity — which is why the status is contested rather than live.
narrative_ontology:disappearance_verdict(marriage_authority__secularist_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__secularist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__secularist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__secularist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__secularist_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored high (0.72) because the program's completion displaces family-law authorship from communities to a legislature the minorities cannot outvote, with the standardization burden falling almost entirely on those whose codes were never consolidated on their own terms — the majority community's law was codified in the 1950s reforms with its own consent, so nominal universal application conceals sharply asymmetric burden placement. Suppression (0.68) reflects the coercive requirement: community refusal cannot be accommodated inside the program's telos, so persistence depends on overriding it through legislative majorities, managed consultation, and eventual compulsion. Suppression is authored as a raw structural property — the engine scales only extractiveness, by directionality and scope. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: the program's coercive machinery lay dormant for four decades, then built sharply in the final stretch (consultation commissions, successive draft codes, state-level enactment, compliance design). Theater_ratio declines across the interval (0.60 to 0.30): early decades the directive principle operated mostly as rhetorical performance, while the contemporary program runs real drafting and enactment machinery, leaving a residual performative share in majoritarian signaling. Accessibility_collapse (0.45) is mid-range: exemptions, opt-out proposals, parallel ceremonial practice, and constitutional litigation keep alternatives partly open, but each enacted code narrows them within its scope. Resistance (0.70) is high and sustained: personal-law boards, regional governments, and community mobilization have blocked national enactment for seven decades. All three series run on one shared six-point grid. gain_flow names secular_modernist_coalition because the program's returns — electoral mobilization, nation-building credit, a code aligned with its members' existing practice — demonstrably accrue there. fixing_cost is prohibitive: the agenda-setting legislature could abandon the telos only by repudiating a founding commitment and rupturing its own coalition's identity, a price exceeding the benefit of retreat.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently. From the minority_religious_communities seat the program presents as an existential override of communal self-governance — the coordination story reads as cover for majoritarian norm-authorship, and that seat's computed type should sit toward the enforced-transfer end. From the secular_modernist_coalition and democratic_legislature seats the same structure presents as unfinished constitutional housekeeping — a coordination project delayed by obstinate particularism — computing toward the coordinated-benefit end. The constitutional_courts seat sees a manageable jurisprudential backlog and a welcome simplification; the excluded women's seat sees her grievance instrumentalized by both sides. The powerless excluded seat's main coalition channel runs through gender_egalitarian_reformers, whose alliance is conditional and whose priorities can detach from hers — currently unrealized, which is precisely what keeps her seat excluded rather than converted into bargaining power. The engine derives these divergences from the declared power, exit, and directional structure; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. secular_modernist_coalition sits near the subsidized end: it collects the uniformity good, its own family law was standardized generations ago on terms it set, and it bears none of the assimilation cost. minority_religious_communities and tribal_customary_law_communities sit near the full-target end: they bear the authorship transfer and the compliance burden, and their exit is closed by religious identity and territorial jurisdiction. democratic_legislature pays little (electoral risk only) while controlling the arrangement, placing it well toward the beneficiary side despite its formal neutrality. constitutional_courts are near-symmetric with a slight subsidy (docket simplification, reduced exposure to selective-intervention accusations). gender_egalitarian_reformers are nominally beneficiaries, but their incidence is the story's open question — the omega variable gender_benefit_incidence holds that uncertainty rather than forcing it into the scalar. minority_community_women are excluded rather than positioned: the derivation cannot place a seat that is kept out of the arrangement's bargaining table, and that exclusion is itself the structural fact the absent_voices answer records.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what prevents mislabeling in both directions. Reading the program as pure coordination (its self-description: neutral modernization completing equal citizenship) erases the asymmetric burden placement; reading it as pure extraction (the communal counter-description: majoritarian assault) erases the genuine problems — forum-shopping, unequal maintenance rights, conflicting succession regimes — that a common code would solve. The R5 interview shows a live-contested mandate, not an atrophied one: the founding problem (post-colonial integration of plural family law) is disputed but not dead, disappearance would rearrange the field, and the mismatch consumer finds no dead-mandate flag (status=contested crossed with verdict=world_rearranges). Piton is excluded structurally: theater_ratio is modest and falling, and a concentrated beneficiary actively maintains the program — the opposite of the no-capturer cost-asymmetry that defines a piton. The mandatrophy risk worth flagging is prospective: if national enactment stalls for another generation while state-level codes accumulate piecemeal, the program could decay into periodic theatrical revival — the theater_ratio trajectory in the measurement series is the early-warning indicator for that decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the secularist reading of the marriage_authority kernel; how would the classification change under the communal_autonomy or federalist_millet sibling readings?',
    'Compare computed classifications across the five sibling stories of the kernel; divergence in beneficiary/victim sets and epsilon locates the disagreement structurally rather than rhetorically.',
    'Under the communal reading, the contested arrangement becomes state override of community-authored norms and the secularist program registers as the suppressive force rather than the coordinate-and-transfer structure; under the federalist reading, pluralism is load-bearing anti-tyranny design and elimination attempts compute as uncompensated extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame omega: this constraint is one reading-indexed instantiation of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    gender_benefit_incidence,
    'Does the uniform code''s promised gender-equality dividend actually accrue to minority-community women, or to the secular-modernist coalition that campaigns in their name?',
    'Post-enactment incidence study: maintenance, divorce, and succession outcomes for minority-community women under the uniform code versus the prior personal-law baseline, disaggregated by community, class, and access-to-court variables.',
    'If incidence confirms coalition capture, the coordination half of the arrangement weakens toward enforced transfer; if minority women measurably gain, the coordination function is genuine and the transfer is the price of standardization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_benefit_incidence, empirical, 'Whether claimed gender-equality benefits reach the invoked population or the campaigning coalition.').

omega_variable(
    transitional_anomaly_status,
    'Is personal-law pluralism actually a transitional residue decaying toward uniformity, as the reading asserts, or a stable equilibrium that persists indefinitely without the code?',
    'Longitudinal convergence analysis: measure inter-code divergence in marriage, divorce, and succession rules across the interval; convergence without compulsion supports the transitional thesis, persistent divergence refutes it.',
    'If pluralism is a stable equilibrium, the transitional-anomaly premise fails, the program loses its historical-inevitability warrant, and the compulsion component computes as extraction without transition logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transitional_anomaly_status, empirical, 'Empirical status of the transitional-anomaly premise underlying the reading.').

omega_variable(
    zero_sum_vs_complementarity,
    'Is the relationship between the secularist program and the communal_autonomy reading genuinely zero-sum (one must eliminate the other), or can layered authority accommodate both?',
    'Institutional test: whether optional-civil-code designs (uniform code as default with community opt-in) sustain both readings without collapse; observe jurisdictions experimenting with optional formats.',
    'If complementarity holds, this reading''s foreclosure edge to the communal sibling is overstated and the arrangement migrates toward ordinary coordination; if zero-sum holds, escalation dynamics dominate and per-seat divergence widens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(zero_sum_vs_complementarity, conceptual, 'Whether the reading contest is structurally zero-sum or admits layered accommodation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__secularist_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ma_secularist_tr_t0, marriage_authority__secularist_reading, theater_ratio, 0, 0.6).
narrative_ontology:measurement(ma_secularist_tr_t15, marriage_authority__secularist_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement(ma_secularist_tr_t30, marriage_authority__secularist_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(ma_secularist_tr_t45, marriage_authority__secularist_reading, theater_ratio, 45, 0.42).
narrative_ontology:measurement(ma_secularist_tr_t60, marriage_authority__secularist_reading, theater_ratio, 60, 0.36).
narrative_ontology:measurement(ma_secularist_tr_t75, marriage_authority__secularist_reading, theater_ratio, 75, 0.3).

% Extraction over time
narrative_ontology:measurement(ma_secularist_be_t0, marriage_authority__secularist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ma_secularist_be_t15, marriage_authority__secularist_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(ma_secularist_be_t30, marriage_authority__secularist_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(ma_secularist_be_t45, marriage_authority__secularist_reading, base_extractiveness, 45, 0.56).
narrative_ontology:measurement(ma_secularist_be_t60, marriage_authority__secularist_reading, base_extractiveness, 60, 0.64).
narrative_ontology:measurement(ma_secularist_be_t75, marriage_authority__secularist_reading, base_extractiveness, 75, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(ma_secularist_su_t0, marriage_authority__secularist_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(ma_secularist_su_t15, marriage_authority__secularist_reading, suppression_requirement, 15, 0.22).
narrative_ontology:measurement(ma_secularist_su_t30, marriage_authority__secularist_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement(ma_secularist_su_t45, marriage_authority__secularist_reading, suppression_requirement, 45, 0.34).
narrative_ontology:measurement(ma_secularist_su_t60, marriage_authority__secularist_reading, suppression_requirement, 60, 0.46).
narrative_ontology:measurement(ma_secularist_su_t75, marriage_authority__secularist_reading, suppression_requirement, 75, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__secularist_reading, resource_allocation).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'who controls marriage law in a plural society' conflates five structurally distinct claims with different epsilon values, beneficiary sets, and failure modes. This file is the secularist member; it links to all four siblings via affects_constraints. The secularist reading functions as upstream pressure on the judicial_harmonization and gender_rights siblings (its legislative-completion demand reframes their incremental strategies as inadequate substitutes) and as the zero-sum antagonist of the communal_autonomy sibling; the federalist_millet sibling competes with it without logical elimination. Cross-file comparison of computed classifications is the resolution mechanism for the kernel_reading_position omega.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
