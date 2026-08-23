% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__incoherent_bundle_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__incoherent_bundle_reading
 *   human_readable: Edo-Period Shinbutsu-Shugo as an Institutionally Managed Ambiguous Bundle
 *   domain: religious_studies/japanese_history/political_theology
 *
 * SUMMARY:
 *   From the Tokugawa settlement to the Meiji Restoration, kami worship and
 *   Buddhism operated through combined shrine-temple complexes under an
 *   officially endorsed trace-manifestation doctrine, backed by compulsory
 *   parish registration and licensed suppression of nativist alternatives.
 *   This story instantiates ONE reading of the contested kernel
 *   shinbutsu_coexistence_commitment: the incoherent_bundle_reading, on which
 *   the arrangement never possessed a stable ontological or doctrinal core —
 *   it was a working bundle of practices whose categorical questions were
 *   systematically left unasked, held together by institutional power that
 *   profited from the blur, and whose rapid collapse under the 1868
 *   separation edicts revealed the missing core rather than creating it. KEY
 *   AGENTS (by structural relationship): tokugawa_bakufu — agenda setter and
 *   secondary beneficiary (institutional/constrained), runs the registration
 *   grid and collects order from the blur; established_buddhist_schools —
 *   primary beneficiary (organized/constrained), collects parish revenues,
 *   funeral monopoly, and lands; shaso_dual_clergy — local
 *   administrator-beneficiary (organized/identity_locked), enforces the
 *   combined form daily and exists only inside it; peasant_danka_households —
 *   primary target (powerless/trapped), pays fees, levies, and labor with no
 *   exit from the parish system; village_festival_communities — collective
 *   participant-beneficiary turned contributor (organized/trapped);
 *   independent_shrine_lineages — subordinated target
 *   (moderate/identity_locked); nativist_kokugaku_scholars — excluded
 *   objector (moderate/constrained), persecuted dissent outside the
 *   settlement; court_shrine_authorities — secondary beneficiary
 *   (moderate/constrained), licenses shrine offices and profits from
 *   unsettled categories. EPSILON REFERENT: per the kernel-reading rule,
 *   extractiveness is authored for the STANDING Edo arrangement as this
 *   reading assesses it — the mature pre-1868 bundle with its resource flows,
 *   enforced doctrinal silence, and policed alternatives — never for the
 *   post-Meiji settlement this reading implicitly contrasts it with. The
 *   sibling readings (syncretic_fusion_reading, domain_partition_reading) are
 *   separate constraints with their own epsilon values and victim sets; they
 *   appear here only as linked family members, not as hedges inside this
 *   story's classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.7).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.58).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "Edo-Period Shinbutsu-Shugo as an Institutionally Managed Ambiguous Bundle").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "religious_studies/japanese_history/political_theology").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'a755edc5-26e2-4b5c-aee9-8c6d182909c2').
narrative_ontology:cs_kernel_codification('a755edc5-26e2-4b5c-aee9-8c6d182909c2', distributed).
narrative_ontology:cs_authority_grounding('a755edc5-26e2-4b5c-aee9-8c6d182909c2', extraction).
narrative_ontology:cs_interpretation_layer_present('a755edc5-26e2-4b5c-aee9-8c6d182909c2').
narrative_ontology:cs_reading_relation('a755edc5-26e2-4b5c-aee9-8c6d182909c2', shinbutsu_coexistence_commitment__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('a755edc5-26e2-4b5c-aee9-8c6d182909c2', shinbutsu_coexistence_commitment__domain_partition_reading, forecloses).
narrative_ontology:cs_axiom('a755edc5-26e2-4b5c-aee9-8c6d182909c2', foundational, no_stable_ontological_commitment).
narrative_ontology:cs_axiom_status(no_stable_ontological_commitment, holdable).
narrative_ontology:cs_axiom_grounding('a755edc5-26e2-4b5c-aee9-8c6d182909c2', no_stable_ontological_commitment, empirically_contingent).
narrative_ontology:cs_axiom('a755edc5-26e2-4b5c-aee9-8c6d182909c2', secondary, persistence_via_enforced_ambiguity).
narrative_ontology:cs_axiom_status(persistence_via_enforced_ambiguity, holdable).
narrative_ontology:cs_axiom_grounding('a755edc5-26e2-4b5c-aee9-8c6d182909c2', persistence_via_enforced_ambiguity, empirically_contingent).
narrative_ontology:cs_reference_frame('a755edc5-26e2-4b5c-aee9-8c6d182909c2', institutionally_managed_ambiguity).
narrative_ontology:cs_drift_state('a755edc5-26e2-4b5c-aee9-8c6d182909c2', meiji_bunri_edicts, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('a755edc5-26e2-4b5c-aee9-8c6d182909c2', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, tokugawa_bakufu).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, established_buddhist_schools).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shaso_dual_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, court_shrine_authorities).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, peasant_danka_households).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, independent_shrine_lineages).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, nativist_kokugaku_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, village_festival_communities).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, village_festival_communities).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__incoherent_bundle_reading, honji_suijaku_official_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers religion through the combined shrine-temple network: the temple registration system compels every household to belong to a Buddhist parish, turning the network into a census and a screen against Christianity. It collects order and information from the arrangement without ever settling what kami and Buddhas are to each other; adjudicating that question would break the registration grid, so the government preserves the blur.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, tokugawa_bakufu, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, tokugawa_bakufu, beneficiary).

% Operate the great temple complexes, hold parish rights over danka households, monopolize funerals and memorial rites, and hold tax-favored land. Many head temples physically contain shrine halls and issue gongen titles for local kami. They recite the official trace-manifestation doctrine when ceremony requires it and otherwise avoid systematic theology that would invite scrutiny; under government license they police anti-Buddhist nativist writing.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, established_buddhist_schools, beneficiary,
    organized, generational, constrained, national).

% Serve as the resident clergy of combined complexes — installed as temple administrators over shrines, performing Buddhist rites for kami, setting the festival calendar, collecting local dues and offerings. Their office, income, and status exist only inside the combined form; stating plainly whether they serve kami or Buddhas would abolish the office itself.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shaso_dual_clergy, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shaso_dual_clergy, agenda_setter).

% Registered at the village temple by law; pay annual parish fees, funeral and memorial charges, festival levies, and repair labor for both the shrine hall and the temple hall of the same complex. Travel and residence changes require permits routed through the same system. Refusing either half of the complex brings administrative penalty.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, peasant_danka_households, payer,
    powerless, immediate, trapped, local).

% Receive a shared ritual year — spring and autumn festivals, purification observances, funeral infrastructure, disaster mutual aid centered on the combined complex. Fund it through collective dues and corvee labor. When officials later force a choice between the shrine side and the temple side, villages split: some strip the temple buildings for timber, others riot to protect them.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, village_festival_communities, beneficiary,
    organized, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, village_festival_communities, payer).

% Hereditary shrine-priest families whose offices were progressively placed under temple supervision — headships granted to resident monks, kami rites reframed as Buddhist services. Keeping rank and stipend meant accepting the overlay; asserting ancestral autonomy invited suppression. Lineage honor is bound to the shrine, so walking away was not a live option.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, independent_shrine_lineages, payer,
    moderate, generational, identity_locked, regional).

% Noble houses that license shrine priests, grant ranks, and propagate their own doctrinal schemes — including one influential house teaching that kami are primordial and Buddhas the derivatives. They collect licensing fees and court prestige from the arrangement and depend on its unsettled categories to keep a niche open against the far wealthier temple establishment.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, court_shrine_authorities, beneficiary,
    moderate, generational, constrained, national).

% Scholars working to recover an original Japanese way centered on the kami, treating the trace-manifestation doctrine as foreign usurpation. Their books are banned or restricted; leading teachers are arrested in periodic crackdowns; manuscripts circulate hand-copied. They stand outside the official settlement and would dismantle it if they could speak inside it.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, nativist_kokugaku_scholars, excluded,
    moderate, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, nativist_kokugaku_scholars, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__incoherent_bundle_reading, established_buddhist_schools).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__incoherent_bundle_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The combined complex solved concrete problems once, centrally: a single institution provided the festival calendar, purification rites, funerals and memorial care, parish registry, and disaster mutual aid. The customary division of labor — kami for purity, harvest, and this-worldly concerns; Buddhist rites for death and afterlife — ordered village life without requiring anyone to settle what kami and Buddhas ontologically are.
% TRANSFER_FUNCTION: Moves money (annual parish fees, funeral and memorial payments, offerings, licensing fees), labor (festival corvee, complex maintenance), and doctrinal deference (recitation of the official trace-manifestation framing, abstention from categorical questions) from peasant households and village communities to the temple establishment, the dual clergy, the licensed court houses, and — as order, census data, and Christian-surveillance coverage — to the warrior government.
% ABSENT_VOICES: Nativist kokugaku scholars are the clearest absent voice: they objected in writing and were banned, restricted, and periodically arrested, so the settlement's claimed unanimity is in part manufactured by their exclusion. Independent shrine lineages had no forum in which to press for autonomy without forfeiting rank. Ordinary parishioners had no channel through which a doctrinal objection could even be formulated.
% DISAPPEARANCE_RATIONALE: Registration had to be rebuilt from scratch as the modern household register; funeral provision convulsed (communities that suddenly had no ritual death-care scrambled for substitutes); festival calendars were forcibly split along a line that had never previously been drawn; thousands of combined complexes were demolished or gutted within a few years; and the state had to construct an entirely new national cult to occupy the vacated ceremonial space.
% FOUNDING_PROBLEM: Integrate the indigenous kami cult with imported Buddhist doctrine and institutions so that both could operate across the same territory without zero-sum conflict — and, subsequently, give the warrior government a single administrative-religious grid for census, surveillance, and control.
% FOUNDING_PROBLEM_CORROBORATION: No corroborating seat comes from inside the arrangement's beneficiary set. The nativist scholarly corpus (Motoori-line and Hirata-school writings) attests from outside that the fusion never rested on a defensible doctrinal settlement; Meiji reformers' own separation-edict memorials invoke an alleged ancient purity as warrant; and modern academic historiography (Kuroda Toshio's kenmitsu-system studies and the scholarship following him) concludes that the arrangement operated without a stable doctrinal kernel — while a rival school of historians continues to dispute that conclusion, which is why the status is recorded as contested rather than dead.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__incoherent_bundle_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claim and the metrics are authored independently. Claimed type tangled_rope expresses what this reading believes structurally true: the arrangement had a genuine, heavily-used coordination function (shared ritual year, death care, registration, disaster aid) AND asymmetric extraction riding the same structure (parish fees, funeral monopoly rents, corvee, doctrinal deference extracted from people with no exit), AND it required continuous active enforcement — compulsory registration, permitted travel, censorship, periodic arrests — without which it did not survive. Metrics are authored descriptively: extractiveness 0.70 and suppression 0.58 describe the mature regime (roughly the 1830-1850 plateau); theater_ratio 0.47 reflects a system whose core rituals still functioned but whose maintenance activity increasingly consisted of performing doctrinal unity it could not cash out — gongen title inflation, ritual escalation, licensed polemic — rather than delivering the underlying services. Accessibility collapse is moderate (0.52): exclusive alternatives were legally and socially unavailable inside the order, yet they never fully collapsed — nativism survived hand-to-hand manuscript circulation, and the explosive voluntary participation in post-1868 stripping suggests large latent demand the enforcement had been holding down. Resistance 0.45 captures persistent low-grade friction: banned scholarship, occasional peasant protest against temple exactions, resentful shrine lineages — short of open revolt while enforcement held. The measurement series runs on ONE shared grid (1603-1868, eight points, all three metrics authored at every point). Extraction climbs monotonically as the parish economy matures; theater crosses 0.5 in the final phase, the Goodhart signature of a system increasingly maintaining its proxy (the appearance of settled harmony) instead of its function. Suppression_requirement follows a U-shape: high during early consolidation, lowest at mid-period normalization, rising again as the nativist challenge forced renewed policing (the 1840s arrests), then collapsing to 0.14 at 1868 when the enforcing regime itself fell — which is the story's evidentiary center: the bundle did not erode, it evaporated once enforcement did. The scalar base_properties values intentionally describe the standing regime, not the terminal cliff; the terminal measurements date the collapse honestly rather than backdating the end-state. Victim-coalition potential stayed low throughout: the three target groups were separated by status, geography, and incompatible aims (peasant households wanted lighter dues, not doctrinal revolution; nativists wanted doctrinal revolution, not lower fees; shrine lineages wanted autonomy within the system), so suppression faced no unified front.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute materially different classifications from identical structural facts. From peasant_danka_households (powerless, trapped) the arrangement presents as enforced extraction: compulsory fees, a funeral monopoly priced above exit, labor owed to two halls for one complex. From the bakufu seat it presents as infrastructure it built and administers; from the temple schools as legitimate parish economy; from village communities as a mixed account — real services received, rising costs paid. The sharpest divergence sits at shaso_dual_clergy: a declared beneficiary whose identity_locked exit pushes its computed position toward the target end anyway, because an agent fused with the arrangement bears its risks without being able to renegotiate them. Identity-lock mechanisms differ by seat and matter for the computation: the dual clergy carry INSTITUTIONAL identity fusion (the office is constituted by the combination; there is no 'pure' version of themselves to exit into); independent_shrine_lineages carry RELATIONAL/lineage fusion (ancestral honor bound to the shrine makes resignation unthinkable); nativist_kokugaku_scholars exhibit the mirror image — ideological fusion locking them INTO opposition, unable to take the arrangement's settlements seriously even where pragmatically useful. When the Meiji edicts broke the institutional frame in 1868, the locks broke with it — which is precisely why the collapse was simultaneous across seats rather than staggered.
 *
 * DIRECTIONALITY LOGIC:
 *   Structural declarations drive the derivation. The bakufu, temple schools, dual clergy, and court houses are declared beneficiaries: the constraint subsidizes them (order, revenues, offices, licensing fees), placing them near the beneficiary end of directionality, damped further for the constrained-but-secure bakufu. Peasant households, shrine lineages, and nativist scholars are declared victims: the arrangement extracts from them (fees, labor, autonomy, voice), and their exits are trapped or identity_locked or constrained — pushing them toward the full-target end. Village_festival_communities are deliberately stakeholder-declared only, not listed in the beneficiary array: their net position trends from beneficiary toward payer over the interval (services genuinely consumed early; dues and corvee escalating as complexes expanded), and the dual-position stakeholder entry plus the measurement series carry that drift better than a static beneficiary declaration would. Suppression is authored as a raw structural property — censorship, compulsory registration, arrest — and is NOT scaled by power or scope; only extractiveness is scaled, by directionality and the national scope of the verification problem (a nationwide doctrinal blur is expensive to verify and therefore cheap to exploit). No directionality overrides are used: the beneficiary/victim declarations plus exit atoms produce the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Reading the Meiji destruction backward tempts two errors: romanticizing the bundle as pure coordination (a rope) because its collapse tore real communal fabric — ignoring that the fees, monopolies, and silences were real extraction people shed eagerly once permitted; or condemning it as a pure snare because its persistence depended on coercion — ignoring that villages genuinely consumed the ritual year, death care, and mutual aid it provided, and that the coordination functions had to be painfully rebuilt after the edicts. Neither pure category survives contact with the record: the same structure coordinated and extracted, and enforcement was load-bearing for both. On the genealogy interview, the founding problem's status is contested rather than dead-and-zombie: the arrangement was not a mandatrophy corpse walking during its life — it delivered its functions until the enforcement regime fell — but the deeper question this reading raises is whether the founding problem was ever solved at all, or only suspended by refusal to ask it. The residual shells (shrines repurposed as state-cult nodes) constitute a DIFFERENT post-1868 arrangement outside this constraint's referent and are not classified here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (incoherent_bundle_reading) of the kernel shinbutsu_coexistence_commitment; what structural facts change if a sibling reading (syncretic_fusion_reading or domain_partition_reading) is adopted instead?',
    'Sibling stories authored independently with their own epsilon, beneficiary/victim structure, and metrics; comparison across the family via network links rather than hedging inside this file. The disagreement is located in whether the arrangement carried stable ontological content.',
    'The fusion reading would lower extractiveness (devotion is genuine, not coerced) and shrink the victim set; the partition reading would lower theater_ratio (the division of labor is principled, not performed) and raise coordination-function weight. This file''s classification holds only under this reading''s premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame omega recording that this story is one indexed reading of a three-reading kernel contest.').

omega_variable(
    internalized_vs_structural_suppression,
    'Was the enforced silence about categorical questions purely structural (compulsory registration, censorship, arrests), or partially internalized (practitioners experiencing no contradiction because practical pluralism felt native)?',
    'Differential post-1868 behavior: villages that stripped temples within months indicate the enforcement was load-bearing (external suppression dominated); regions that rioted to protect complexes or quietly maintained combined practice indicate internal incorporation. Proportion estimated from regional variation in the haibutsu kishaku record.',
    'A high internalized share means the arrangement''s suppressive force would have partially survived enforcement collapse — raising effective persistence beyond the structural measure and softening the collapse-velocity evidence; a low share confirms the reading''s claim that the bundle held only by external force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural versus internalized mechanism behind the arrangement''s enforced doctrinal silence.').

omega_variable(
    strategic_vs_emergent_ambiguity,
    'Was the ambiguity deliberately maintained by agents who profited from non-resolution, or an emergent equilibrium no one designed?',
    'Institutional behavior at moments when categorical challenges arose: the bakufu''s repeated refusal to adjudicate shrine-temple disputes on doctrinal grounds, the temple schools'' investment in deflecting rather than answering nativist critique, the court houses'' propagation of competing hierarchies (including the inversion teaching kami as primordial) that preserved every party''s niche — consistent strategic deflection indicates design.',
    'Deliberate maintenance strengthens the extraction reading of the enforcement apparatus (agents purchased the blur because resolution destroyed rents); emergent equilibrium would weaken the ''deliberate'' clause and shift blame from agency to path dependence without changing the type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_vs_emergent_ambiguity, empirical, 'Whether the bundle''s ambiguity was engineered or drifted into.').

omega_variable(
    counterfactual_survival_without_meiji,
    'Would the bundle have persisted indefinitely absent the Meiji state''s coercive intervention — and if so, does the 1868 collapse measure inherent incoherence or merely the destruction of a viable-enough system by an external shock?',
    'Counterfactual analysis against comparable un-forced trajectories: late-Edo internal strain indicators (rising anti-Buddhist sentiment among commoners, shrine-lineage defections, nativist manuscript circulation rates) extrapolated against enforcement capacity of a hypothetical surviving bakufu.',
    'A durable-counterfactual result undermines the reading''s central delta (collapse as revelation) and reattributes the breakdown to the shock; a strained-counterfactual result confirms the bundle was already failing and the edicts only timed the fall.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_survival_without_meiji, conceptual, 'Counterfactual survival test for the ''collapsing under Meiji pressure'' claim.').

omega_variable(
    revealed_vs_created_incoherence,
    'How much of the violence and velocity of the post-1868 anti-Buddhist destruction reflects incoherence the bundle always contained, versus incoherence manufactured by the new state''s need for a purified national cult with an identifiable enemy?',
    'Compare destruction intensity across regions varying in state pressure but similar in prior combined-practice depth; examine whether popular stripping preceded or followed official encouragement in each locality.',
    'Precedent of popular action supports ''reveal'' (the reading''s claim); uniform state-led destruction supports ''create,'' shifting causal weight from the bundle''s structure to the successor regime''s construction and weakening this reading''s evidentiary advantage over the siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revealed_vs_created_incoherence, conceptual, 'Attribution of collapse violence: latent incoherence versus successor-regime manufacture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 1603, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t1603, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1603, 0.18).
narrative_ontology:measurement_basis(shin_tr_t1603, observed).
narrative_ontology:measurement(shin_tr_t1650, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1650, 0.24).
narrative_ontology:measurement_basis(shin_tr_t1650, observed).
narrative_ontology:measurement(shin_tr_t1700, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1700, 0.29).
narrative_ontology:measurement_basis(shin_tr_t1700, observed).
narrative_ontology:measurement(shin_tr_t1750, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1750, 0.34).
narrative_ontology:measurement_basis(shin_tr_t1750, observed).
narrative_ontology:measurement(shin_tr_t1800, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1800, 0.39).
narrative_ontology:measurement_basis(shin_tr_t1800, observed).
narrative_ontology:measurement(shin_tr_t1830, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1830, 0.44).
narrative_ontology:measurement_basis(shin_tr_t1830, observed).
narrative_ontology:measurement(shin_tr_t1850, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1850, 0.49).
narrative_ontology:measurement_basis(shin_tr_t1850, observed).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1868, 0.57).
narrative_ontology:measurement_basis(shin_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t1603, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1603, 0.4).
narrative_ontology:measurement_basis(shin_be_t1603, observed).
narrative_ontology:measurement(shin_be_t1650, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1650, 0.47).
narrative_ontology:measurement_basis(shin_be_t1650, observed).
narrative_ontology:measurement(shin_be_t1700, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1700, 0.53).
narrative_ontology:measurement_basis(shin_be_t1700, observed).
narrative_ontology:measurement(shin_be_t1750, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1750, 0.58).
narrative_ontology:measurement_basis(shin_be_t1750, observed).
narrative_ontology:measurement(shin_be_t1800, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1800, 0.63).
narrative_ontology:measurement_basis(shin_be_t1800, observed).
narrative_ontology:measurement(shin_be_t1830, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1830, 0.68).
narrative_ontology:measurement_basis(shin_be_t1830, observed).
narrative_ontology:measurement(shin_be_t1850, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1850, 0.71).
narrative_ontology:measurement_basis(shin_be_t1850, observed).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1868, 0.3).
narrative_ontology:measurement_basis(shin_be_t1868, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t1603, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1603, 0.55).
narrative_ontology:measurement_basis(shin_su_t1603, observed).
narrative_ontology:measurement(shin_su_t1650, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1650, 0.49).
narrative_ontology:measurement_basis(shin_su_t1650, observed).
narrative_ontology:measurement(shin_su_t1700, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1700, 0.45).
narrative_ontology:measurement_basis(shin_su_t1700, observed).
narrative_ontology:measurement(shin_su_t1750, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1750, 0.46).
narrative_ontology:measurement_basis(shin_su_t1750, observed).
narrative_ontology:measurement(shin_su_t1800, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1800, 0.51).
narrative_ontology:measurement_basis(shin_su_t1800, observed).
narrative_ontology:measurement(shin_su_t1830, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1830, 0.57).
narrative_ontology:measurement_basis(shin_su_t1830, observed).
narrative_ontology:measurement(shin_su_t1850, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1850, 0.61).
narrative_ontology:measurement_basis(shin_su_t1850, observed).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1868, 0.14).
narrative_ontology:measurement_basis(shin_su_t1868, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__incoherent_bundle_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment__domain_partition_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'shinbutsu-shugo' into three epsilon-invariant readings of one kernel, per the epsilon-invariance principle: the label conflates three structurally distinct commitments (ontological unification via honji suijaku; stable domain partition without unification; no stable content, only enforced ambiguity). Each reading is a separate story with its own epsilon, beneficiaries, victims, and classification. The syncretic fusion story is upstream (the official doctrine was cited as evidence by every downstream arrangement and by the bakufu's administrative reliance on it); this incoherent-bundle story is downstream-contested, treating the upstream doctrine as a performed surface over an empty core. Both sibling files should carry reciprocal links and a parallel note documenting the decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
