% ============================================================================
% CONSTRAINT STORY: marriage_authority__gender_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__gender_rights_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: marriage_authority__gender_rights_reading
 *   human_readable: Patriarchal Personal Law Authority under Gender-Rights Constitutional Challenge
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   This constraint story instantiates the GENDER-RIGHTS READING of the
 *   contested marriage-authority kernel. The reading asserts that personal
 *   law authority over marriage, as practiced in multi-religious India,
 *   operates as a snare specifically on gender grounds: it extracts women's
 *   autonomy (particularly unmarried women choosing marriage, married women
 *   choosing divorce or property rights, widows seeking remarriage) through
 *   asymmetric personal law codes that privilege male authority and religious
 *   institutional authority. The reading does NOT dispute the value of
 *   religious diversity or community autonomy in general; it disputes whether
 *   that diversity must be purchased at the cost of women's constitutional
 *   equality. Judicial expansion of constitutional equality guarantees —
 *   imposed case-by-case on practices like instant triple talaq, maintenance
 *   denial, and custody assumption — is the reform mechanism. This reading
 *   COEXISTS with other readings (communal autonomy, federalist millet,
 *   secularist, judicial harmonization) held by different parties; it does
 *   not foreclose them. It INFLUENCES them by shifting what is treated as
 *   negotiable within the tradition (e.g., once courts invalidate triple
 *   talaq, communal-autonomy advocates must either accept women's consent
 *   requirement or argue for formal exception — the space has shifted). The
 *   constraint is claimed as a SNARE because women victims have no exit
 *   option other than identity exit (leaving the community), and the
 *   suppression mechanism is both structural (legal confinement to personal
 *   law codes) and internalized (family honor, community belonging fused with
 *   acceptance of the rules).
 *
 * KEY AGENTS:
 *   - women_within_patriarchal_personal_law: powerless, identity-locked, structural victims of asymmetric marital rights
 *   - male_heads_of_household: moderate power, constrained exit, structural beneficiaries of authority concentration
 *   - religious_authorities_adjudicating_personal_law: institutional agenda-setters, claim naturality and unchangeability of the rules
 *   - women_rights_advocates: organized beneficiaries of judicial expansion, partly excluded from personal law adjudication
 *   - apex_court: institutional observer, carves judicial boundaries on case-by-case basis
 *   - democratic_legislature: institutional observer, non-action sustains the constraint despite formal power to reform
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, 0.82).
domain_priors:suppression_score(marriage_authority__gender_rights_reading, 0.71).
domain_priors:theater_ratio(marriage_authority__gender_rights_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__gender_rights_reading, snare).
narrative_ontology:human_readable(marriage_authority__gender_rights_reading, "Patriarchal Personal Law Authority under Gender-Rights Constitutional Challenge").
narrative_ontology:topic_domain(marriage_authority__gender_rights_reading, "legal/constitutional").

domain_priors:requires_active_enforcement(marriage_authority__gender_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__gender_rights_reading, 'a6557908-ea81-439b-bd62-65f7280e8d6c').
narrative_ontology:cs_kernel_codification('a6557908-ea81-439b-bd62-65f7280e8d6c', formalized).
narrative_ontology:cs_authority_grounding('a6557908-ea81-439b-bd62-65f7280e8d6c', lineage).
narrative_ontology:cs_interpretation_layer_present('a6557908-ea81-439b-bd62-65f7280e8d6c').
narrative_ontology:cs_reading_relation('a6557908-ea81-439b-bd62-65f7280e8d6c', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6557908-ea81-439b-bd62-65f7280e8d6c', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('a6557908-ea81-439b-bd62-65f7280e8d6c', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6557908-ea81-439b-bd62-65f7280e8d6c', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('a6557908-ea81-439b-bd62-65f7280e8d6c', foundational, gender_equality_non_negotiable_within_tradition).
narrative_ontology:cs_axiom_status(gender_equality_non_negotiable_within_tradition, holdable).
narrative_ontology:cs_axiom_grounding('a6557908-ea81-439b-bd62-65f7280e8d6c', gender_equality_non_negotiable_within_tradition, deontological).
narrative_ontology:cs_axiom('a6557908-ea81-439b-bd62-65f7280e8d6c', secondary, women_consent_requirement_marital_dissolution).
narrative_ontology:cs_axiom_status(women_consent_requirement_marital_dissolution, holdable).
narrative_ontology:cs_axiom_grounding('a6557908-ea81-439b-bd62-65f7280e8d6c', women_consent_requirement_marital_dissolution, empirically_contingent).
narrative_ontology:cs_reference_frame('a6557908-ea81-439b-bd62-65f7280e8d6c', religious_tradition_as_law_source).
narrative_ontology:cs_drift_state('a6557908-ea81-439b-bd62-65f7280e8d6c', constitutional_equality_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a6557908-ea81-439b-bd62-65f7280e8d6c', '').
narrative_ontology:cs_kernel_id(marriage_authority__gender_rights_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, male_heads_of_household).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, religious_authorities_adjudicating_personal_law).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, religious_minorities_under_majoritarian_pressure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, women_rights_advocates).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, secular_reformers_and_ucc_advocates).
narrative_ontology:constraint_vindicates(marriage_authority__gender_rights_reading, constitutional_equality_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority__gender_rights_reading, gender_as_suspect_classification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Governed by personal law codes (religious or customary) that grant asymmetric marital rights: unilateral male divorce (triple talaq in Islamic law), unequal inheritance, restricted custody over children, limited property autonomy. Exit from the personal law system means exit from the community itself — identity, kinship, social standing are constituted through belonging. The constraint operates through both structural barriers (legal recognition only within the code, no remedy outside it) and internalized norms (family pressure, honor dynamics, fear of ostracization). Judicial intervention via constitutional equality grounds targets specific practices but does not dissolve the overall authority structure.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law, payer,
    powerless, biographical, identity_locked, national).

% Benefit from the asymmetric distribution of marital powers: unilateral divorce rights, default guardianship of children, property claims as family head. The constraint grants them authority within the personal law system that they would lose under secular codification or under constitutional equality doctrine. They do not set the rules — religious authorities and legislatures do — but the rules benefit their structural position. As judicial intervention restricts specific powers (e.g., banning triple talaq without cause), their effective authority narrows while the constraint's formal existence persists.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, male_heads_of_household, beneficiary,
    moderate, biographical, constrained, national).

% Adjudicate family law disputes within their community, interpreting religious texts and tradition. The state delegates enforcement to them, granting them interpretive authority. They present the rules as divinely ordained or ancient custom — natural and unchangeable — and defend them as essential to community identity. Judicial expansion of constitutional equality grounds challenges their authority to define what is non-negotiable within the code. Some religious authorities resist reinterpretation; others claim they are reforming from within tradition. The constraint's persistence depends on their continued adjudicatory role.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, religious_authorities_adjudicating_personal_law, agenda_setter,
    institutional, generational, mobile, national).

% Campaign for constitutional equality guarantees applied to personal law; bring test cases seeking judicial invalidation of specific practices; lobby legislatures for reform. They benefit from any judicial decision that narrows the personal law's scope or imposes constitutional floors. They frame the issue as women's fundamental rights constrained by religious/communal authority that privileges male power. They are partly excluded from the adjudicatory process itself — they appear as interveners in litigation and as legislative witnesses, not as interpreters of the personal law codes. Their power comes from organized advocacy and constitutional framing, not from structural position within the system.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_rights_advocates, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__gender_rights_reading, women_rights_advocates, excluded).

% Advocate for elimination of personal law pluralism via Uniform Civil Code, framing the constraint as an artifact of colonial fragmentation. They benefit from any judicial decision that expands constitutional floors, as it incrementally moves toward their end-goal (formal secular codification). However, they are distinct from women's rights advocates on the reading axis: they push for secular elimination of the personal law system itself, while the gender-rights reading works within personal law to constrain its most extractive practices. Both coalitions push judicial expansion, but for different terminal reasons.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, secular_reformers_and_ucc_advocates, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__gender_rights_reading, secular_reformers_and_ucc_advocates, excluded).

% Interprets the constitutional equality guarantee and decides whether it overrides personal law authority on specific issues (marriage dissolution, property, custody). The court sits between the constraint's persistence and its erosion: each decision either reaffirms personal law's domain (sustaining the constraint) or expands constitutional reach into it (narrowing the constraint). The court faces institutional pressure from both directions — defend pluralism and community autonomy, or enforce national constitutional floors. No single decision dissolves the constraint; each carves out a domain.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, apex_court, observer,
    institutional, generational, analytical, national).

% Would argue that personal law authority is integral to community identity and self-determination; that constitutional equality doctrine imposes majoritarian secular values on minority traditions; that the constraint is not extraction but cultural survival. They are excluded from this reading's framework — the reading does not engage their framing, only disputes their authority on gender grounds. In other readings (communal_autonomy_reading, federalist_millet_reading) they are central.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, religious_and_communal_leaders, excluded,
    institutional, generational, constrained, national).

% Could formally reform personal law through legislation, but political difficulty (communal mobilization, coalition logic) has kept major reform off the agenda. Judicial expansion via constitutional grounds may shift the legislative dynamic by altering what is treated as settled or contestable. The legislature does not directly enforce the constraint, but its non-action sustains it.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, democratic_legislature, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__gender_rights_reading, religious_authorities_adjudicating_personal_law).
narrative_ontology:fixing_cost_class(marriage_authority__gender_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Personal law authority coordinates community identity and marital norms: a married person is not merely in a contract, but embedded in a community-recognized status with defined rights, duties, and inheritance consequences. The system solves the problem of how a diverse, multi-religious polity recognizes marriage across different understandings of what marriage IS (sacrament, contract, kinship transfer) by delegating definition to communities themselves rather than imposing a single secular code.
% TRANSFER_FUNCTION: Transfers authority over a woman's life trajectory — when/whether to marry, divorce rights, custody claims, property autonomy, legal identity — from women themselves to male family heads and male-dominated religious authorities. The constraint extracts the woman's independent decision-making power and concentrates it in male hands and institutional hands, justified by cultural/religious tradition. Judicial intervention transfers limited domains (e.g., right to divorce without male consent, property protection) back to women, narrowing the extraction.
% ABSENT_VOICES: Religious and communal leaders defending the cultural autonomy reading are excluded from this particular reading's frame — they would argue the constraint is not extraction but cultural integrity and that judicial intervention imposes majoritarian secular values. Secular reformers pushing for complete Uniform Civil Code are also outside the frame, though aligned on pushing judicial expansion. Women within patriarchal personal law often cannot voice objection within the system itself — family and community pressure, fear of ostracization, identity-fusion with the community suppress internal dissent. The constraint's suppression mechanism includes silencing their own voice.
% DISAPPEARANCE_RATIONALE: If this constraint vanished — if personal law authority over marriage dissolved and secular constitutional equality replaced it — women would gain independent legal status, inheritance rights, and divorce rights; communities would lose the ability to enforce internal norms through law; men within those communities would lose the authority this constraint grants. The entire legal infrastructure of personal law adjudication would either dissolve or be radically reconstituted. Marriages would still exist, but the state's recognition of marriage would no longer be mediated through communal/religious authority.
% FOUNDING_PROBLEM: Post-colonial legal pluralism: independent India inherited a fragmented personal law system (Muslim law, Hindu law, Christian law, Sikh law, Parsi law) from colonial delegation to 'native law' courts. The system was designed to accommodate religious diversity without majoritarian imposition of a single civil code. Early independence, the constraint was framed as protecting minority communities from majoritarian Hindu-code projects and ensuring cultural continuity.
% FOUNDING_PROBLEM_CORROBORATION: Religious authorities attest the founding problem is still live — that secular Uniform Civil Code would erase minority traditions and constitute majoritarian imposition. Women's rights advocates and secular reformers attest the founding problem is solved (India is stable, secular principles are accepted) and the constraint persists as patriarchal extraction riding on the cover of pluralism. Constitutional courts, in intermediate positions, have ruled that protecting cultural diversity does not require protecting gender inequality — the founding problem (religious minority protection) is distinct from the current problem (women's rights suppression). The evidence from outside the religious-authority seat supports the shift in reading.
narrative_ontology:disappearance_verdict(marriage_authority__gender_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__gender_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__gender_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__gender_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__gender_rights_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__gender_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__gender_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint transfers substantial autonomy-decision-making from women to male family heads and religious authorities, justified by religious/cultural tradition. The measurement trajectory is flat-rising: the extractiveness does not decline when courts invalidate triple talaq or mandate maintenance — it persists because the overall personal law structure remains intact and controls other domains (inheritance, custody, property, marital status). Suppression is moderately high (0.71) because the constraint operates through both legal confinement (women have no judicial remedy outside personal law codes that govern them) and internalized suppression (honor dynamics, family pressure, identity-fusion with community make exit unthinkable, even when legal routes theoretically exist). Theater ratio is moderate-low (0.28) — the constraint has genuine coordination function (communities value personal law authority; women's identity is constituted within communal frameworks), but an increasing share of enforcement activity defends the male asymmetry against judicial encroachment, not the legitimate coordination problem itself. As courts invalidate specific practices (triple talaq 2017, maintenance grounds 2020), the religious authorities must devote more energy to defending remaining asymmetries, and the ratio of theater to function rises.
 *
 * PERSPECTIVAL GAP:
 *   The male-head-of-household and religious-authority seats should compute this as coordination (protecting cultural identity, solving the multi-religious-pluralism problem). The women-within-personal-law seat computes it as snare (extraction of autonomy through legal confinement and identity-lock). The apex-court and women's-rights-advocate seats compute it as snare with contested boundaries (some practices are genuine coordination cost, others are pure extraction; courts must adjudicate which is which, case by case). The gender-rights reading PRIVILEGES the women's-rights and court seats over the communal-authority seats. Other readings reverse this priority. The engine computes per-seat classification from the stakeholder positions and structural data; the committer frame (this reading) is authored into the omega variables and the cs_structure fields, not into the metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Women_within_patriarchal_personal_law are full targets (d near 1.0): the constraint extracts their autonomy, suppression is both structural and internalized, exit is identity-locked, they have no choice set outside the personal law system as it is currently structured. Male heads sit near 0.3–0.4: they benefit from the asymmetry but do not set the rules; they have constrained exit (formal reform would require either personal choice to accept secular marriage or legislative action). Religious authorities sit near 0.15: they interpret and apply rules but are increasingly constrained by judicial review; they claim they are not beneficiaries but custodians of tradition. Women's-rights advocates sit near 0.0: they are excluded and organized, pushing reform; they benefit from judicial expansion but do not extract directly. The court sits at 0.5: it is neutral-positioned but its decisions allocate the constraint's boundaries. This directionality divergence is the engine's read of the structural data; it is not tuned by the author.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (religious minority protection in a multi-religious polity) is CONTESTED in status. The constraint was designed to solve it — personal law delegation preserves community identity without majoritarian domination. But women's-rights advocates and courts increasingly argue the founding problem is solved (India is stable, secular principles are accepted; minorities are protected by constitutional rights and democratic law) while the constraint persists in defending gender inequality. The snare classification prevents the false-positive rope reading: if the constraint were classified as rope (genuine coordination), it would imply that the gender-asymmetric outcomes are necessary to the pluralism solution — but courts have shown that communities can maintain identity while accepting women's consent to divorce and maintaining women's property rights. The snare classification means the coordination-to-extraction ratio must be false or has shifted; mandatrophy analysis asks whether the founding problem has died while the constraint persists. Measurement series show extractiveness stable while judicial boundary-carving increases — the constraint adjusts its operation but does not dissolve. This is the signature of a constraint whose founding problem is dead but whose beneficiaries (male authority, religious institutional authority) defend it theatrically under the cover of pluralism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) primarily structural (legal confinement to personal law codes, no appeal to secular courts, institutional barriers to exit) or internalized (women''s internalized acceptance of the rules, fused identity with community, honor dynamics, family pressure that persists even when legal barriers are removed)?',
    'Post-reform data: if women exit personal law when legal routes open (e.g., in jurisdictions or communities where formal secular marriage is permitted), the suppression was primarily structural. If suppression persists even when exit becomes legally available (women remain within personal law despite secular option, or face severe social consequences for leaving), the suppression is substantially internalized.',
    'If primarily structural, the constraint''s effective suppression can be reduced by expanding secular legal alternatives (lowering the exit cost). If substantially internalized, formal legal reform alone is insufficient; the constraint''s operation is rooted in identity-fusion and family relations that legal remedy cannot directly address. The classification would shift from snare (pure extraction via coercion) toward a hybrid snare-tangled-rope (coercion + identity coordination so entangled that exit itself is unthinkable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural legal confinement or internalized family/identity dynamics (or both, and in what proportion)').

omega_variable(
    founding_problem_survival_vs_mandatrophy,
    'Has the founding problem (religious-minority protection in a majoritarian democracy) genuinely been solved, or does it persist as a live structural need?',
    'Constitutional-historical analysis: has India''s secular constitutional framework adequately protected religious-minority rights WITHOUT delegating family law authority? Are there documented cases where religious minorities lost rights BECAUSE personal law pluralism was withdrawn? Alternatively, do documented cases show that when personal law is reformed or withdrawn, minority-religious dignity is adequately protected by constitutional non-discrimination and freedom-of-religion clauses?',
    'If the founding problem is solved and the constraint persists, it is a case of mandatrophy: a constraint whose justifying condition has expired but whose beneficiaries (male authority, religious institutional authority) defend it under the original cover story. Judicial expansion can then be understood as remedying an obsolete constraint, not as threatening legitimate pluralism. If the founding problem persists, the constraint retains legitimacy as anti-tyranny mechanism, and reform must be more careful to preserve pluralism while narrowing gender extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_survival_vs_mandatrophy, empirical, 'Whether the constraint''s founding problem (minority protection) persists as a structural need or has been superseded by constitutional protections').

omega_variable(
    judicial_boundary_carving_vs_dissolution,
    'Can judicial expansion of constitutional equality grounds continuously narrow the personal law constraint without eventually dissolving it entirely? Or is there a structural limit where case-by-case boundary-carving reaches a threshold beyond which the personal law system loses coherence?',
    'Empirical-historical observation: examine the trajectory of case-by-case judicial boundary-setting across other jurisdictions (e.g., South Africa, Indonesia, Egypt, Nigeria). At what point does a constraint become so eroded that its institutional form disintegrates, even if formal repeal never occurs? Conversely, do constraints sometimes stabilize at reduced scope (personal law for property and will-execution, but not marital rights)?',
    'If there is no dissolution threshold — if the constraint can stabilize at reduced scope indefinitely — then judicial expansion is the mechanism by which the snare is converted to a more-limited rope or scaffold (narrowed extraction with remaining coordination function). If a dissolution threshold exists, then judicial expansion will eventually trigger either formal reform or institutional collapse of the personal law system, and the current-state classification (snare with high extraction) may be temporary/transitional rather than stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_boundary_carving_vs_dissolution, conceptual, 'Whether judicial boundary-carving has a structural limit, or can stabilize the constraint at reduced scope indefinitely').

omega_variable(
    committer_frame_gender_rights_vs_secularist,
    'This reading frames the issue as gender-rights-within-pluralism; a sibling secularist reading frames it as pluralism-itself-as-problem. Do these readings coexist, or does one logically foreclose the other within a single framework?',
    'Conceptual-logical analysis: can a framework simultaneously hold that (a) religious/cultural diversity is legitimate AND (b) secular codification is desirable, if the secular code enforces gender equality? The gender-rights reading says yes — accept pluralism but enforce equality within it. The secularist reading says no — pluralism is inherently patriarchal and must be eliminated. These are different normative stances. They coexist in the corpus (held by different parties), but within a single adjudicatory framework, do they compete for dominance, or can both principles be honored?',
    'If the readings coexist (different parties hold them, neither rules out the other within the other''s own framework), then judicial expansion can proceed incrementally, narrowing the gender-extractive scope without dissolving pluralism. If one foreecloses the other, then India''s ultimate trajectory on personal law reform is determined by which reading wins structural dominance — and the current state is transitive, not stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_gender_rights_vs_secularist, conceptual, 'Whether gender-rights-within-pluralism and secular-elimination-of-pluralism foreclose each other, or coexist as live readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__gender_rights_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__gender_rights_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t8, marriage_authority__gender_rights_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement_basis(marr_tr_t8, observed).
narrative_ontology:measurement(marr_tr_t16, marriage_authority__gender_rights_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement_basis(marr_tr_t16, observed).
narrative_ontology:measurement(marr_tr_t25, marriage_authority__gender_rights_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement_basis(marr_tr_t25, observed).
narrative_ontology:measurement(marr_tr_t35, marriage_authority__gender_rights_reading, theater_ratio, 35, 0.27).
narrative_ontology:measurement_basis(marr_tr_t35, observed).
narrative_ontology:measurement(marr_tr_t50, marriage_authority__gender_rights_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(marr_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__gender_rights_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t8, marriage_authority__gender_rights_reading, base_extractiveness, 8, 0.79).
narrative_ontology:measurement_basis(marr_be_t8, observed).
narrative_ontology:measurement(marr_be_t16, marriage_authority__gender_rights_reading, base_extractiveness, 16, 0.8).
narrative_ontology:measurement_basis(marr_be_t16, observed).
narrative_ontology:measurement(marr_be_t25, marriage_authority__gender_rights_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement_basis(marr_be_t25, observed).
narrative_ontology:measurement(marr_be_t35, marriage_authority__gender_rights_reading, base_extractiveness, 35, 0.82).
narrative_ontology:measurement_basis(marr_be_t35, observed).
narrative_ontology:measurement(marr_be_t50, marriage_authority__gender_rights_reading, base_extractiveness, 50, 0.82).
narrative_ontology:measurement_basis(marr_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__gender_rights_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t8, marriage_authority__gender_rights_reading, suppression_requirement, 8, 0.67).
narrative_ontology:measurement_basis(marr_su_t8, observed).
narrative_ontology:measurement(marr_su_t16, marriage_authority__gender_rights_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(marr_su_t16, observed).
narrative_ontology:measurement(marr_su_t25, marriage_authority__gender_rights_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(marr_su_t25, observed).
narrative_ontology:measurement(marr_su_t35, marriage_authority__gender_rights_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(marr_su_t35, observed).
narrative_ontology:measurement(marr_su_t50, marriage_authority__gender_rights_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(marr_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__gender_rights_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority__gender_rights_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% The marriage-authority kernel decomposes into five constraint stories, each instantiating a different reading of what authority should govern personal law in a multi-religious polity and how that authority should treat gender. The gender-rights reading (this file) is one specific constraint — the snare operating on women through patriarchal personal law practices, enforced by religious authorities and defended by appeals to cultural diversity. It coexists with communal-autonomy, federalist-millet, secularist, and judicial-harmonization readings, all of which have different ε values, different stakeholder structures, and different classifications. The readings are linked: each reading's success influences the others by shifting what is treated as negotiable within tradition and what institutional reforms are demanded. Judicial expansion of constitutional equality (favored by gender-rights and secularist readings) creates pressure on federated-millet and judicial-harmonization readings to accommodate women's rights while preserving community autonomy. The communal-autonomy reading is most directly threatened by judicial expansion, making it coexist-with rather than foreclose the gender-rights reading — both remain live, but judicial victories for the gender-rights reading narrow the communal-autonomy reading's operational scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__gender_rights_reading, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
