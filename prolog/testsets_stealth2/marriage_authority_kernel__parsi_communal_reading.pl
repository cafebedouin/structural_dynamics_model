% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__parsi_communal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__parsi_communal_reading, []).

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
 *   constraint_id: marriage_authority_kernel__parsi_communal_reading
 *   human_readable: Parsi Communal Matrimonial Authority (Parsi Marriage and Divorce Act 1936)
 *   domain: comparative law / constitutional pluralism / religious governance
 *
 * SUMMARY:
 *   The Parsi communal reading instantiates one reading of the
 *   marriage-authority kernel: matrimonial and membership authority for
 *   Parsis derives from community custom as codified in the Parsi Marriage
 *   and Divorce Act 1936, administered through community delegate tribunals
 *   and trustee bodies rather than the generic civil judiciary. The
 *   arrangement coordinates a genuine collective problem — a tiny, declining,
 *   ethnoreligious minority maintains identity boundaries (admission via
 *   navjote, endogamy norms) and internal dispute resolution through
 *   institutions only it can staff — while it bears asymmetrically on women
 *   who marry outside the community and on their children, who lose fire
 *   temple, dokhma burial, and trust access under a rule applied
 *   patrilineally (children of Parsi fathers are admissible; children of
 *   Parsi mothers are not, absent trustee dispensation). The Goolrokh Gupta
 *   litigation (Gujarat High Court 2012; Supreme Court 2022, dismissed as
 *   academic with the validity question left open) marks the live
 *   constitutional challenge to the gender-asymmetric application. This story
 *   is one member of a five-reading constraint family —
 *   hindu_codified_reading, muslim_shariat_reading,
 *   christian_canonical_reading, and secular_civil_reading are separate
 *   stories linked via network.affects_constraints — and epsilon here is
 *   authored for THIS reading's standing arrangement only, per the
 *   epsilon-invariance principle. The claim/metric relationship is
 *   deliberate: claimed_type is authored from structural analysis (genuine
 *   coordination plus asymmetric extraction plus active enforcement), and the
 *   metrics are authored from the arrangement's actual operation; the engine
 *   computes each seat's type from the structural data.
 *
 * KEY AGENTS:
 *   - community_trustees: agenda setter (institutional/constrained) — administers membership, trusts, and the litigation defense of exclusion rules
 *   - parsi_priesthood: primary beneficiary (organized/identity_locked) — hereditary ritual gatekeepers of admission
 *   - endogamous_parsi_households: primary beneficiary (organized/identity_locked) — collective holders of the boundary-maintenance gains
 *   - parsi_women_in_exogamous_marriages: primary target (moderate/identity_locked) — bear the gender-asymmetric exclusion
 *   - children_of_exogamous_parsi_mothers: secondary target (powerless/trapped) — excluded from membership by default
 *   - parsi_matrimonial_delegates: enforcement seat (organized/constrained) — lay jury adjudicating under the 1936 Act
 *   - nonparsi_spouses: excluded party (moderate/mobile) — their marriages trigger rules they have no seat in
 *   - secular_constitutional_courts: analytical observer (institutional/analytical) — adjudicate the open constitutional challenge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__parsi_communal_reading, 0.48).
domain_priors:suppression_score(marriage_authority_kernel__parsi_communal_reading, 0.52).
domain_priors:theater_ratio(marriage_authority_kernel__parsi_communal_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__parsi_communal_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__parsi_communal_reading, "Parsi Communal Matrimonial Authority (Parsi Marriage and Divorce Act 1936)").
narrative_ontology:topic_domain(marriage_authority_kernel__parsi_communal_reading, "comparative law / constitutional pluralism / religious governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__parsi_communal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__parsi_communal_reading, '1b6e8015-25f5-4938-917d-f5279434b2db').
narrative_ontology:cs_kernel_codification('1b6e8015-25f5-4938-917d-f5279434b2db', formalized).
narrative_ontology:cs_authority_grounding('1b6e8015-25f5-4938-917d-f5279434b2db', lineage).
narrative_ontology:cs_interpretation_layer_present('1b6e8015-25f5-4938-917d-f5279434b2db').
narrative_ontology:cs_reading_relation('1b6e8015-25f5-4938-917d-f5279434b2db', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b6e8015-25f5-4938-917d-f5279434b2db', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b6e8015-25f5-4938-917d-f5279434b2db', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b6e8015-25f5-4938-917d-f5279434b2db', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('1b6e8015-25f5-4938-917d-f5279434b2db', foundational, communal_custom_supremacy_in_marriage).
narrative_ontology:cs_axiom_status(communal_custom_supremacy_in_marriage, holdable).
narrative_ontology:cs_axiom_grounding('1b6e8015-25f5-4938-917d-f5279434b2db', communal_custom_supremacy_in_marriage, conventional).
narrative_ontology:cs_axiom('1b6e8015-25f5-4938-917d-f5279434b2db', foundational, endogamy_preserves_communal_identity).
narrative_ontology:cs_axiom_status(endogamy_preserves_communal_identity, holdable).
narrative_ontology:cs_axiom_grounding('1b6e8015-25f5-4938-917d-f5279434b2db', endogamy_preserves_communal_identity, empirically_contingent).
narrative_ontology:cs_reference_frame('1b6e8015-25f5-4938-917d-f5279434b2db', codified_communal_matrimonial_autonomy).
narrative_ontology:cs_drift_state('1b6e8015-25f5-4938-917d-f5279434b2db', contemporary_demographic_decline, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1b6e8015-25f5-4938-917d-f5279434b2db', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_priesthood).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, community_trustees).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, endogamous_parsi_households).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_women_in_exogamous_marriages).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, children_of_exogamous_parsi_mothers).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__parsi_communal_reading, communal_autonomy_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__parsi_communal_reading, ethnoreligious_boundary_preservation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected trustees of the Bombay Parsi Punchayet and anjuman committees who administer charitable trusts, fire temples, and dokhmas, and decide admission cases case-by-case. They set membership practice and litigated through 2022 to defend the exclusion of women who marry outside the community. They answer to the endogamous majority that elects them; resignation from trusteeship is possible, but exit from the community whose institutions they run is not.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, community_trustees, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__parsi_communal_reading, community_trustees, beneficiary).

% Hereditary ritual specialists who officiate navjote admission, weddings, and funerary rites. Admission of children of Parsi fathers runs through their ceremonies, and the ceremonial economy and the community's ritual boundary-keeping flow through them. The vocation is open only to sons of priestly families, so leaving it means leaving the only role their caste can hold.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_priesthood, beneficiary,
    organized, generational, identity_locked, regional).

% Households that marry within the community. They hold access to fire temples, dokhma burial, and trust benefits, and their children are admitted by default. Their collective identity continuity depends on the boundary the membership rules maintain; their marriage market is the small community itself, which they experience as both the point of the rules and their demographic cost.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, endogamous_parsi_households, beneficiary,
    organized, generational, identity_locked, national).

% Lay co-religionists elected to the delegate panels that adjudicate Parsi marriages and divorces under the 1936 Act's jury system. They serve terms, apply codified custom, and return to ordinary life afterward; their adjudicatory authority exists only inside the communal forum and lapses with it.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_matrimonial_delegates, agenda_setter,
    organized, biographical, constrained, regional).

% Parsi women married to non-Parsi men. The trusteeship treats them as having left the community: they lose fire temple and dokhma access and trust benefits, while their own self-understanding remains Parsi — Goolrokh Gupta litigated for a decade to be recognized as what she insisted she was. Their children are excluded by default. There is no procedure for re-entry, and un-becoming Parsi has no meaning inside an ethnoreligious identity.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_women_in_exogamous_marriages, payer,
    moderate, biographical, identity_locked, national).

% Children of Parsi mothers and non-Parsi fathers. They are not admitted by default, unlike children of Parsi fathers, and admission requires trustee dispensation they cannot claim as of right. They grow up inside the community's social world while its institutions do not recognize them, and they had no part in the marriage that triggered their exclusion.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, children_of_exogamous_parsi_mothers, payer,
    powerless, biographical, trapped, national).

% Non-Parsi husbands and wives of Parsis. Their marriages are the triggering fact for the exclusion rules, but they hold no seat in community governance, no admission path, and no vote in the trusteeship that decided the rules their families live under.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, nonparsi_spouses, excluded,
    moderate, biographical, mobile, national).

% The Gujarat High Court and the Supreme Court of India, which hear constitutional challenges (Articles 14, 15, 21, 25) to the exclusion practice. The High Court ruled against the litigant in 2012; the Supreme Court in 2022 dismissed the appeal as academic without deciding the validity question, leaving the challenge open. They adjudicate the dispute and hold no stake in the communal boundary itself.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, secular_constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__parsi_communal_reading, diffuse).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__parsi_communal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the membership boundary and internal dispute resolution of a small, declining, ethnoreligious minority: marriage solemnization, admission of children (navjote), matrimonial adjudication by community delegates familiar with Parsi custom, and trustee-administered access to fire temples, dokhmas, and charitable trusts all run through communal institutions rather than the generic civil judiciary.
% TRANSFER_FUNCTION: Moves adjudicatory authority over Parsi marriages and divorces from the civil judiciary to community delegate tribunals, and moves communal standing — temple, burial, and trust access — away from women who marry outside the community and from their children, preserving the endogamous boundary for those who remain inside it.
% ABSENT_VOICES: Non-Parsi spouses have no seat in community governance though their marriages trigger the exclusion rules; children of exogamous Parsi mothers are governed by admission rules they cannot vote on; Parsi women's groups litigate from outside the trusteeship. The trusteeship that decides membership is elected by and answerable to the endogamous majority, so the seats most burdened by the rules are the least represented in the room where the rules are defended.
% DISAPPEARANCE_RATIONALE: Matrimonial disputes would move to family courts within months; admission and exclusion disputes would become ordinary civil litigation against the trusts; and the community would need a new institutional form — redefined trusteeship or a membership association — to keep boundary-keeping, fire temple and dokhma access rules, and trust administration. The delegate system would not be rebuilt quickly, because its staff is the community itself.
% FOUNDING_PROBLEM: Nineteenth-century Parsi matrimonial disputes were heard by civil judges unfamiliar with Parsi custom; the Parsi Marriage and Divorce Acts of 1865 and 1936 created delegate (lay jury) tribunals so Parsis could be judged on marriage and divorce by co-religionists applying codified custom, protecting communal autonomy for a statutorily recognized minority.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: Parsi litigants who have taken matrimonial disputes to civil forums, the Gujarat High Court's Goolrokh Gupta judgment (2012, subjecting the exclusion practice to constitutional scrutiny), and non-community demographers working from census data attest that the adjudication function is transferable and that demographic pressure is reshaping the arrangement's viability; the trustees, from inside, attest the boundary-keeping function remains live. No source outside the benefiting parties attests that the delegate tribunal specifically remains necessary.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__parsi_communal_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__parsi_communal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__parsi_communal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__parsi_communal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__parsi_communal_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__parsi_communal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__parsi_communal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__parsi_communal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.48: real but concentrated — most community members are net beneficiaries of identity coordination, while the exclusion rule takes communal standing from a litigated minority (exogamous women and their children); the value rises over the interval as exogamous marriage became more common and the rule reached more families. Suppression 0.52: enforcement runs through institutional denial of access (temple, burial, trust), litigation defense of the rule, and identity fusion — being Parsi is ethnoreligious, so exit is unavailable as self-re-identification; the force is partly structural and partly internalized, an ambiguity carried by the omega variable rather than resolved in the scalar. Theater 0.22: the delegate tribunal system genuinely adjudicates; the performative share rises modestly as the shrinking population maintains institutions beyond their caseload. Accessibility_collapse 0.40: the Special Marriage Act 1954 is a live alternative channel, so alternatives do not fully collapse, but choosing the alternative carries the communal cost the membership rules enforce. Resistance 0.55: nearly a decade of constitutional litigation plus internal women's advocacy. Suppression is authored as a raw structural property, unscaled; only extractiveness is scaled downstream by directionality and scope. All three temporal series share one grid (t=0,15,30,45,60,75,90 over 1936–2026), and the suppression series is authored because the story specifically traces an enforcement ratchet: as the community shrinks, the stakes of each boundary decision rise and the trusteeship's defensive litigation hardened across 2012–2022.
 *
 * PERSPECTIVAL GAP:
 *   From the trusteeship and priesthood seats the arrangement presents as legitimate identity coordination — the boundary IS the community, and maintaining it is survival for a population that halved since 1941. From the exogamous women's seat the same structure presents as gender-asymmetric exclusion enforced by institutions they cannot re-enter and cannot stop belonging to. From the constitutional courts' seat it presents as a pending equality question (Articles 14 and 15) that the 2022 Supreme Court left open. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Trustees and the priesthood sit near the beneficiary end: the arrangement subsidizes their authority and ritual gatekeeping, and the trustees additionally hold agenda-setting power over admission (declared as dual-positioned). Endogamous households are collective beneficiaries with identity-locked exit — low d, damped effective extraction. Exogamous women and their children are the targets: they bear the transfer (loss of communal standing), and their exit options — identity_locked and trapped respectively — sit them toward the full-target end of d, amplifying effective extraction for exactly the seats with the least power. The delegates administer enforcement without declared beneficiary status; their seat is computed from their enforcement position. Non-Parsi spouses are excluded rather than coordinated — the rule's operation is defined against them. The constitutional courts are analytical observers with no extraction exposure. No directionality overrides are authored: the beneficiary/victim declarations plus exit options produce the correct structural relationships for every seat, and the coarse power-atom override surface would misfire across same-power seats with different roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — matrimonial adjudication by custom-familiar lay delegates because civil judges did not know Parsi custom — is partly obsolete: family courts now exist and Parsi litigants have used civil forums. But the arrangement's second function, boundary-keeping for a declining minority, is live, so the mandate has not fully outlived its function and mandatrophy is not resolved. The contested founding-problem status paired with a world_rearranges disappearance verdict signals a hybrid rather than a zombie: the adjudication shell could be retired without rearranging much, while the boundary function would rearrange the community's institutional life if removed. Reading the arrangement as pure extraction would erase the real coordination most members experience; reading it as pure coordination would erase the litigated exclusion of women and children. The tangled_rope claim holds both, and the diffuse gain flow plus prohibitive fixing cost (the fixers — trustees and legislature — face communal-schism and identity-dissolution costs they treat as existential) is explained by the live coordination function, not by an atrophied one: this is not a piton, because the function has not atrophied — it is actively defended and actively contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marriage_authority_kernel_reading_contest,
    'This constraint is the parsi_communal_reading of the marriage_authority_kernel: what structurally changes under the sibling readings (hindu_codified_reading, muslim_shariat_reading, christian_canonical_reading, secular_civil_reading), and where exactly is the disagreement located?',
    'Comparative classification across the five sibling stories. The disagreement is located in the source of matrimonial authority — communal custom vs codified religious law vs constitutional individual right — which moves the victim set (communal exclusion exists only under the communal readings), the enforcement seat (community delegates vs civil courts), and the exit structure (identity-locked under ethnoreligious readings, mobile under the secular reading).',
    'Under secular_civil_reading the communal exclusion victims vanish (there is no communal membership to lose) and extraction drops toward coordination cost; under the other religious readings the victim sets differ with each community''s exclusion rules. Resolution is cross-reading comparison, never within-reading reclassification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(marriage_authority_kernel_reading_contest, conceptual, 'Committer structure: one of five readings of the marriage-authority kernel; sibling readings change the victim set, enforcement seat, and exit structure.').

omega_variable(
    endogamy_demographic_viability,
    'Does endogamous boundary maintenance preserve the Parsi community or accelerate its decline? The community roughly halved between the 1941 and 2011 censuses under endogamy, late marriage, and low fertility.',
    'Demographic modeling of admission-policy scenarios (status-quo endogamy vs patrilineal vs bilateral admission) against census and community survey data; the community-commissioned demographic studies and independent census analysis are the available baselines.',
    'If exclusion demonstrably accelerates decline, the reading''s foundational endogamy axiom loses its empirical warrant (it is authored empirically_contingent) and the axiom_overriding drift becomes severe; if endogamy is demographically neutral or protective, the axiom holds and the extraction challenge stands alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogamy_demographic_viability, empirical, 'Whether the endogamy axiom''s empirical foundation survives the community''s demographic trajectory.').

omega_variable(
    gender_asymmetry_separability,
    'Is the patrilineal admission rule (children of Parsi fathers admissible, children of Parsi mothers not) intrinsic to the communal custom, or an interpretive accretion separable from endogamy itself?',
    'Historical-custom analysis of pre-codification Parsi practice, plus constitutional adjudication of the question the Supreme Court left open in 2022; the trusteeship''s case-by-case admission decisions are the interpretive layer''s own data.',
    'If separable, the arrangement could shed its extraction component while keeping the coordination function — a tangled_rope drifting toward rope; if intrinsic, the exclusion is constitutive of the custom and the constitutional challenge targets the reading''s core premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_asymmetry_separability, empirical, 'Whether the gender-asymmetric exclusion is separable from the endogamy coordination function.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression operating on exogamous Parsi women structural (denial of temple, burial, and trust access enforced by trustees) or internalized (identity fusion — they cannot stop being Parsi and do not seek to)?',
    'Post-exit suppression trajectory: whether women who marry out re-identify outside the community or continue claiming Parsi identity against institutional denial, as Goolrokh Gupta did through a decade of litigation.',
    'If substantially internalized, effective suppression exceeds the structural measure — the exclusion travels with the women after any institutional reform, and access remedies alone would not lift the weight; if structural, access remedies suffice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs internalized mechanism of the exclusion''s suppressive force.').

omega_variable(
    delegate_tribunal_transferability,
    'Would Parsi matrimonial adjudication lose anything irreducible if moved wholesale to civil family courts, or is the delegate system''s custom-familiarity now replicable by trained judges?',
    'Comparative outcomes between Parsi matrimonial cases heard in civil forums (where parties opted out or the Act did not reach) and delegate-tribunal cases, on reversal rates, party satisfaction, and custom-application accuracy.',
    'If transferable, the adjudication shell of the founding problem is dead and the arrangement''s remaining function is boundary-keeping alone, sharpening the mandatrophy mismatch; if not transferable, the tribunal is live coordination infrastructure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(delegate_tribunal_transferability, empirical, 'Whether the delegate tribunal''s adjudication function is transferable to civil courts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__parsi_communal_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t15, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement_basis(marr_tr_t15, observed).
narrative_ontology:measurement(marr_tr_t30, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 30, 0.13).
narrative_ontology:measurement_basis(marr_tr_t30, observed).
narrative_ontology:measurement(marr_tr_t45, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 45, 0.15).
narrative_ontology:measurement_basis(marr_tr_t45, observed).
narrative_ontology:measurement(marr_tr_t60, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 60, 0.17).
narrative_ontology:measurement_basis(marr_tr_t60, observed).
narrative_ontology:measurement(marr_tr_t75, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 75, 0.2).
narrative_ontology:measurement_basis(marr_tr_t75, observed).
narrative_ontology:measurement(marr_tr_t90, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 90, 0.22).
narrative_ontology:measurement_basis(marr_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t15, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement_basis(marr_be_t15, observed).
narrative_ontology:measurement(marr_be_t30, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 30, 0.34).
narrative_ontology:measurement_basis(marr_be_t30, observed).
narrative_ontology:measurement(marr_be_t45, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 45, 0.37).
narrative_ontology:measurement_basis(marr_be_t45, observed).
narrative_ontology:measurement(marr_be_t60, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement_basis(marr_be_t60, observed).
narrative_ontology:measurement(marr_be_t75, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 75, 0.44).
narrative_ontology:measurement_basis(marr_be_t75, observed).
narrative_ontology:measurement(marr_be_t90, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 90, 0.48).
narrative_ontology:measurement_basis(marr_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t15, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 15, 0.34).
narrative_ontology:measurement_basis(marr_su_t15, observed).
narrative_ontology:measurement(marr_su_t30, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 30, 0.37).
narrative_ontology:measurement_basis(marr_su_t30, observed).
narrative_ontology:measurement(marr_su_t45, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 45, 0.4).
narrative_ontology:measurement_basis(marr_su_t45, observed).
narrative_ontology:measurement(marr_su_t60, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 60, 0.44).
narrative_ontology:measurement_basis(marr_su_t60, observed).
narrative_ontology:measurement(marr_su_t75, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 75, 0.48).
narrative_ontology:measurement_basis(marr_su_t75, observed).
narrative_ontology:measurement(marr_su_t90, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 90, 0.52).
narrative_ontology:measurement_basis(marr_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__parsi_communal_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, secular_civil_reading).

% DUAL FORMULATION NOTE:
% Marriage-law authority in India is one colloquial label covering five structurally distinct readings of one kernel (marriage_authority_kernel); per the epsilon-invariance principle each reading is a separate constraint story with its own epsilon, victim set, and enforcement structure. This story instantiates the parsi_communal_reading. The sibling readings are linked via affects_constraints in both directions; the secular_civil_reading is the exit channel whose existence keeps accessibility_collapse below mountain levels, and the constitutional litigation against this reading's exclusion rule is adjudicated in the secular frame — an upstream/downstream pressure relation rather than a logical foreclosure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
